use std::collections::HashMap;

use crate::lexer::Token;
use crate::IdentIdx;
use crate::Int;
use crate::Keyword;
use crate::SetType;

type ASTBox<T> = Box<T>;
pub type ASTBody = Vec<ASTStatement>;

/// Binary operators with precedence as integer representation
/// IMPORTANT: WHEN ADDING NEW BINOPS: CHANGE THE "from_number" FUNCTION!
#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum BinaryOp {
    // numbers specify their precedence. Higher is higher precedence
    // ZERO is RESERVED for algorithm, also -1 is reserved for no more operators
    SetAdd = 1,
    SetSub = 2,
    Set = 3,
    Equals = 4,
    Less = 5,
    Greater = 6,
    Add = 7,
    Sub = 8,
    Multiply = 9,
}

impl BinaryOp {
    fn from_number(from: i8) -> BinaryOp {
        if from < 1 || from > 9 {
            panic!("Can't create binop with prec: {}", from);
        }
        unsafe { std::mem::transmute(from) }
    }
}

/// 0 is start of precedence, > 0 is normal operators. -1 is END OF EXPRESSION don't look further
type BinOpPrec = i8;

impl BinaryOp {
    /// precedence
    fn prec(self) -> BinOpPrec {
        self as _
    }
}

/// holds InAstExpr and token idx of expr
#[derive(Debug)]
pub struct ASTExpr(pub InASTExpr, pub usize);

#[derive(Debug)]
pub enum InASTExpr {
    Int(Int),
    VarName(IdentIdx),
    FunctionCall(IdentIdx, Vec<ASTExpr>), // a function and its arguments
    Binary(BinaryOp, ASTBox<ASTExpr>, ASTBox<ASTExpr>),
}

#[derive(Debug)]
pub struct ASTStatement(pub InASTStatement, pub usize);

#[derive(Debug)]
pub enum InASTStatement {
    If {
        condition: ASTExpr,
        if_true_body: ASTBody,
        if_false_body: ASTBody,
    },
    While {
        condition: ASTBox<ASTExpr>,
        body: ASTBody,
    },
    EvalExpr(ASTExpr), // evaluating an expression: 1+f(2)
    CreateVar(IdentIdx),
    Function {
        name: IdentIdx,
        args: Vec<IdentIdx>, // names of args
        body: ASTBody,
    },
    Return(ASTExpr),
    /// {}
    Block(ASTBody),
}
/// Semantic Token
enum Soken {
    /// one
    Return,
    /// standalone
    Int(i64),
    /// standalone
    Var(IdentIdx),
    /// standalone
    CreateVar(IdentIdx),
    /// Name and nr of args
    /// expects nr of args
    /// the definition comes after, TODO how to know when it ends?
    FuncDef(IdentIdx, u16),
    /// two
    Binop(BinaryOp),
    /// Name and nr args
    /// expects nr of args
    FuncCall(IdentIdx, u16),
    /// expects three
    If,
    /// expects two
    While,
}

struct Parser<'parser_lifetime> {
    tokens: &'parser_lifetime mut std::iter::Peekable<
        std::iter::Enumerate<std::slice::Iter<'parser_lifetime, Token>>,
    >,

    // name and nr of args, later used in veryfying that called functions exist, without doing extra pass
    functions: HashMap<IdentIdx, u16>,
    // semantic tokens
    sokens: Vec<Soken>,
    // parallel array of source code places for the sokens
    origins: Vec<usize>,

    // immutable:
    token_idx_to_char_range: &'parser_lifetime Vec<(usize, usize)>,
    source: &'parser_lifetime str,
    file_name: &'parser_lifetime str,
}

/// returns Abstract Syntax Tree with Function hashmap which contains all functions declarations
pub fn parse(
    tokens: &[Token],
    token_idx_to_char_range: &Vec<(usize, usize)>,
    source: &str,
    file_name: &str,
) -> (ASTBody, HashMap<IdentIdx, u16>) {
    let mut token_iter = tokens.iter().enumerate().peekable();
    let mut parser = Parser {
        tokens: &mut token_iter,
        functions: HashMap::new(),
        sokens: Vec::new(),
        origins: Vec::new(),

        token_idx_to_char_range,
        source,
        file_name,
    };

    let ast = parser.parse_scope();

    (ast, parser.functions)
}
/// takes self, pattern and error message
macro_rules! eat_require {
    ($self:ident, $the_pattern:pat, $msg:literal) => {
        match { $self.eat() } {
            (n, $the_pattern) => n,
            (n, &t) => $self.report_incorrect_semantics($msg, Some(&t), n),
        }
    };
}
/// takes self, pattern and error message
macro_rules! eat_require_get {
    ($self:ident, $the_pattern:path, $msg:literal) => {
        match $self.eat() {
            (n, &$the_pattern(a)) => (n, a),
            (n, &t) => $self.report_incorrect_semantics($msg, Some(&t), n),
        }
    };
}

impl<'parser_lifetime> Parser<'parser_lifetime> {
    fn eat(&mut self) -> (usize, &Token) {
        self.tokens.next().unwrap()
    }

    fn peek(&mut self) -> (usize, &Token) {
        self.tokens.peek().copied().unwrap()
    }

    fn report_incorrect_semantics(
        &mut self,
        msg: &str,
        bad_tok: Option<&Token>,
        token_start_idx: usize,
    ) -> ! {
        crate::mark_error_in_source(
            self.source,
            self.file_name,
            msg,
            self.token_idx_to_char_range[token_start_idx],
        );
        if let Some(bad_tok) = bad_tok {
            println!("\x1b[0m    bad token: `{:?}`", bad_tok);
        } else {
            println!("\x1b[0m    bad token could not be pinpointed unfortunately.");
        }

        std::process::exit(1);
    }

    /// parses scope that has to start with { brace, eats last }
    fn parse_scope_require_start_brace(&mut self) {
        // must be { after this
        eat_require!(
            self,
            Token::Keyword(Keyword::StartBlock),
            "Expected `{` at start of scope"
        );
        self.parse_scope()
    }

    /// Parse generalized tokens. Probably statements. Eats last }
    /// Variable name => Set it to expression
    /// If => generate If node
    /// SHOULD NOT be used so much in code, function parse_scope_require_start_brace is cleaner most of time
    fn parse_scope(&mut self) {
        let mut push_to_statements = |statement: ASTStatement| {
            println!("got an AST: {:?}", statement);
        };

        loop {
            let &(place, &token) = match self.tokens.peek() {
                Some(tok) => tok,
                None => break,
            };
            println!("parse token {:?}", token);
            match &token {
                Token::Keyword(Keyword::FunctionIncoming) => {
                    self.eat();
                    // Next token should be function name identifier
                    let func_name = {
                        let (func_name_place, func_name) = eat_require_get!(
                            self,
                            Token::Identifier,
                            "Expected function name identifier"
                        );
                        if self.functions.contains_key(&func_name) {
                            self.report_incorrect_semantics(
                                "Function with same name already declared!",
                                None,
                                func_name_place,
                            )
                        }
                        func_name
                    };
                    // Next token should be start paren
                    eat_require!(
                        self,
                        Token::Keyword(Keyword::StartParen),
                        "Expected start parenthesis after function name"
                    );
                    // Next should be a series of name and type-names, ending with a end paren
                    let mut args = 0;
                    loop {
                        let arg = self.eat();
                        let arg_name = match *arg.1 {
                            Token::Identifier(arg_name) => arg_name,
                            Token::Keyword(Keyword::EndParen) => break,
                            _ => self.report_incorrect_semantics(
                                "Expected argument name",
                                Some(&arg.1),
                                arg.0,
                            ),
                        };
                        args += 1;
                        self.push(Soken::Var(arg_name), arg.0);
                        // Next should be a comma or end paren
                        match self.eat() {
                            (_, Token::Keyword(Keyword::Comma)) => (),
                            (_, Token::Keyword(Keyword::EndParen)) => break,
                            (n, &t) => self.report_incorrect_semantics(
                                "Expected comma or end parenthesis after argument",
                                Some(&t),
                                n,
                            ),
                        };
                    }
                    self.functions.insert(func_name, args);

                    self.parse_scope_require_start_brace();
                    self.push(Soken::FuncDef(func_name, args), place);
                }
                // create variable with `let`, just lookahead on name, later code will handle rest as expression
                // TODO fix so you can't do let a += 1; or let b + 3;
                &Token::Keyword(Keyword::CreateVar) => {
                    self.eat();
                    // Next token should be variable-name
                    let after = self.peek();
                    let at_end =
                        eat_require_get!(self, Token::Identifier, "expected variable_name");
                    self.push(Soken::CreateVar(at_end.1), at_end.0);
                    // parse expression let (a = 1+2+3)
                    self.parse_expr(); // this outputs expr
                    eat_require!(
                        self,
                        Token::Keyword(Keyword::EndStatement),
                        "statement must end with `;`"
                    );
                }
                // If statement
                Token::Keyword(Keyword::If) => {
                    self.eat();
                    self.parse_expr();
                    self.parse_scope_require_start_brace();
                    // possible Else part
                    let stat = match self.peek().1 {
                        Token::Keyword(Keyword::Else) => {
                            self.eat();
                            self.parse_scope_require_start_brace();
                        }
                        _ => panic!("please add Else after if statemnt TODO remove this"),
                    };
                    self.push(Soken::If, place);
                }
                // While statement
                Token::Keyword(Keyword::While) => {
                    self.eat();
                    self.parse_expr();
                    self.parse_scope_require_start_brace();
                    self.push(Soken::While, place);
                }
                Token::Keyword(Keyword::EndBlock) => {
                    self.eat();
                    return statements;
                }

                // return statement that returns from function with value
                Token::Keyword(Keyword::Return) => {
                    self.eat(); // eat return token
                    self.parse_expr();
                    self.push(Soken::Return, place);
                    eat_require!(
                        self,
                        Token::Keyword(Keyword::EndStatement),
                        "Expected `;` after return expression"
                    );
                }
                Token::Keyword(Keyword::StartBlock) => {
                    let scope_body = self.parse_scope_require_start_brace();
                    push_to_statements(ASTStatement(InASTStatement::Block(scope_body), place));
                }

                // Function call, or just expression
                &Token::Identifier(_) | Token::Int(_) | Token::String(_) => {
                    // parse the expression. e.g. f(x) + 42, or 32 + g(f(x))
                    self.parse_expr();

                    eat_require!(
                        self, // should be semicolon after expression:
                        Token::Keyword(Keyword::EndStatement),
                        "must end statement with `;`!"
                    );
                }
                &t => self.report_incorrect_semantics(
                    "Erroneous token to start statement with",
                    Some(&t),
                    place,
                ),
            };
        }
        statements
    }
    /// add new semantic token, which was found at place
    fn push(&mut self, soken: Soken, place: usize) {
        self.sokens.push(soken);
        self.origins.push(place);
    }
    fn parse_primary(&mut self) {
        let (p, &token) = self.eat();
        let soken = match token {
            Token::String(_) => self.report_incorrect_semantics(
                "Strings not allowed in language currently!",
                None,
                p,
            ),
            Token::Int(e) => Soken::Int(e),
            Token::Keyword(Keyword::StartParen) => {
                self.parse_expr();
                self.eat(); // eat the end parenthesis
                return;
            }
            Token::Identifier(e) => {
                // can either be a variable or a function call
                match self.peek() {
                    (_, Token::Keyword(Keyword::StartParen)) => {
                        self.eat(); // eat start paren
                        let args = self.parse_function_call_args();
                        Soken::FuncCall(e, args)
                    }
                    _ => Soken::Var(e),
                }
            }
            t @ Token::Keyword(_) => {
                self.report_incorrect_semantics("no keywords as primary in expr", Some(&t), p)
            }
        };
        self.push(soken, p);
    }

    /// should be called when the first `(` is eaten. Will eat the last `)`
    /// returns nr of args function was called with
    fn parse_function_call_args(&mut self) -> u16 {
        let mut args = 0;
        // if no args
        if let (_, Token::Keyword(Keyword::EndParen)) = self.peek() {
        } else {
            // eat all arguments
            loop {
                self.parse_expr();
                args += 1;

                match self.peek() {
                    (_, Token::Keyword(Keyword::EndParen)) => break,
                    (_, Token::Keyword(Keyword::Comma)) => {
                        self.eat(); // eat comma
                    }
                    (n, &t) => self.report_incorrect_semantics("expected comma", Some(&t), n),
                }
            }
        }
        self.eat(); // eat end paren
        args
    }

    /// doesn't eat end of expression
    fn parse_expr(&mut self) {
        // TODO: add prefix unary operator parsing here
        self.parse_primary();

        return self.parse_binop_and_right(0);
    }

    fn parse_binop_and_right(&mut self, prev_prec: BinOpPrec) {
        loop {
            let (our_prec, p) = self.parse_oper();
            // if find binop that is LESS binding, have to go back and create expr for this like going from  * to +
            if our_prec < prev_prec {
                return; // also, if reached end -> prec will be -1, so goes back
            }
            self.eat(); // eat operator

            // get right hand side
            self.parse_primary();
            // peek at operator after right hand side
            let (next_prec, _) = self.parse_oper();
            // now we have (a+b), though if the precedence AFTER binds tighter, this is actually a + (b * ...)
            if next_prec > our_prec {
                self.parse_binop_and_right(our_prec + 1);
            }
            // merge left and right
            self.push(Soken::Binop(BinaryOp::from_number(our_prec)), p);
        }
    }

    /// returns precedence of binop and it's token index.
    /// DOES NOT EAT TOKEN
    /// if ` ; ) } ` ... returns -1 (special precedence)
    fn parse_oper(&mut self) -> (BinOpPrec, usize) {
        let (res_place, &res_token) = self.peek();
        use BinaryOp as B;
        use Keyword as K;
        use Token::Keyword as T;
        let prec = match res_token {
            T(K::Plus) => B::Add.prec(),
            T(K::Minus) => B::Sub.prec(),
            T(K::Multiply) => B::Multiply.prec(),
            T(K::Less) => B::Less.prec(),
            T(K::More) => B::Greater.prec(),
            T(K::Equals) => B::Equals.prec(),
            T(K::Set(SetType::Add)) => B::SetAdd.prec(),
            T(K::Set(SetType::Sub)) => B::SetSub.prec(),
            T(K::Set(SetType::Set)) => B::Set.prec(),
            T(K::EndParen) | T(K::EndStatement) | T(K::StartBlock) | T(K::Comma) => -1, // -1 precedence, end of expr
            _ => self.report_incorrect_semantics(
                &format!(
                    "What are you doing? Can't just put token `{:?}` here!",
                    res_token
                ),
                Some(&res_token),
                res_place,
            ),
        };
        (prec, res_place)
    }
}
