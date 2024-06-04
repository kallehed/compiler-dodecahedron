use std::collections::HashMap;

use crate::lexer::Token;
use crate::IdentIdx;
use crate::Int;
use crate::Keyword;
use crate::SetType;

type ASTBox<T> = Box<T>;
pub type ASTBody = Vec<ASTStatement>;

/// Binary operators with precedence as integer representation
#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum BinaryOp {
    // numbers specify their precedence. Higher is higher precedence
    // ZERO is RESERVED for algorithm,
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

/// 0 is start of precedence, > 0 is normal operators. -1 is END OF EXPRESSION don't look further
type BinOpPrecedence = i8;

impl BinaryOp {
    fn precedence(self) -> BinOpPrecedence {
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

/// TODO fix so some exprs are not boxed? Seems like that could be done. Don't know why they are
/// boxed
#[derive(Debug)]
pub enum InASTStatement {
    If {
        condition: ASTExpr,
        body: ASTBody,
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

struct Parser<'parser_lifetime> {
    tokens: &'parser_lifetime mut std::iter::Peekable<
        std::iter::Enumerate<std::slice::Iter<'parser_lifetime, Token>>,
    >,

    // name and nr of args
    functions: HashMap<IdentIdx, u16>,

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

        token_idx_to_char_range,
        source,
        file_name,
    };

    let ast = parser.parse_scope();

    (ast, parser.functions)
}
macro_rules! eat_require {
    ($self:ident, $the_pattern:pat, $msg:literal) => {
        match { $self.eat().unwrap() } {
            (n, $the_pattern) => n,
            (n, &t) => $self.report_incorrect_semantics($msg, Some(&t), n),
        }
    };
}
macro_rules! eat_require_get {
    ($self:ident, $the_pattern:path, $msg:literal) => {
        match $self.eat().unwrap() {
            (n, &$the_pattern(a)) => (n, a),
            (n, &t) => $self.report_incorrect_semantics($msg, Some(&t), n),
        }
    };
}

impl<'parser_lifetime> Parser<'parser_lifetime> {
    fn eat(&mut self) -> Option<(usize, &Token)> {
        self.tokens.next()
    }

    fn peek(&mut self) -> Option<(usize, &Token)> {
        self.tokens.peek().copied()
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

    /// Parse generalized tokens. Probably statements.
    /// Variable name => Set it to expression
    /// If => generate If node
    fn parse_scope(&mut self) -> ASTBody {
        let mut statements = ASTBody::new();

        let mut push_to_statements = |statement: ASTStatement| {
            println!("got an AST: {:?}", statement);
            statements.push(statement)
        };

        loop {
            let (place, &token) = match self.peek() {
                Some(tok) => tok,
                None => break,
            };
            println!("token {:?}", token);
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
                    let mut arg_vec = Vec::new();
                    loop {
                        let arg_name = match self.eat().unwrap() {
                            (_, &Token::Identifier(arg_name)) => arg_name,
                            (_, Token::Keyword(Keyword::EndParen)) => break,
                            (tok_nr, &tok) => self.report_incorrect_semantics(
                                "Expected argument name",
                                Some(&tok),
                                tok_nr,
                            ),
                        };
                        arg_vec.push(arg_name);
                        // Next should be a comma or end paren
                        match self.eat().unwrap() {
                            (_, Token::Keyword(Keyword::Comma)) => (),
                            (_, Token::Keyword(Keyword::EndParen)) => break,
                            (n, &t) => self.report_incorrect_semantics(
                                "Expected comma or end parenthesis after argument",
                                Some(&t),
                                n,
                            ),
                        };
                    }
                    // must be { after this
                    eat_require!(
                        self,
                        Token::Keyword(Keyword::StartBlock),
                        "Expected `{` after function declaration"
                    );
                    self.functions.insert(func_name, arg_vec.len() as _);

                    push_to_statements(ASTStatement(
                        InASTStatement::Function {
                            name: func_name,
                            args: arg_vec,
                            body: self.parse_scope(),
                        },
                        place,
                    ));
                }
                // create variable with `let`
                &Token::Keyword(Keyword::CreateVar) => {
                    self.eat();
                    // Next token should be variable-name
                    let (var_token_place, var_idx) =
                        eat_require_get!(self, Token::Identifier, "expected variable name");
                    // make sure there is a SET token after var name
                    let set_token_place = eat_require!(
                        self,
                        Token::Keyword(Keyword::Set(SetType::Set)),
                        "expected set token"
                    );
                    // now there should be an expression
                    let expr = ASTBox::new(self.parse_expr());
                    // it should end with a semicolon
                    eat_require!(
                        self,
                        Token::Keyword(Keyword::EndStatement),
                        "expected `;` after variable declaration"
                    );
                    push_to_statements(ASTStatement(InASTStatement::CreateVar(var_idx), place)); // create variable

                    // TODO: maybe use some sentinel value for token idx of EvalExpr. Current value
                    // is probably useless later 2024-5-12
                    push_to_statements(ASTStatement(
                        InASTStatement::EvalExpr(ASTExpr(
                            InASTExpr::Binary(
                                BinaryOp::Set,
                                ASTBox::new(ASTExpr(InASTExpr::VarName(var_idx), var_token_place)),
                                expr,
                            ),
                            set_token_place,
                        )),
                        set_token_place,
                    ));
                }
                // If statement
                Token::Keyword(Keyword::If) => {
                    self.eat();
                    let cond = self.parse_expr();
                    eat_require!(
                        self,
                        Token::Keyword(Keyword::StartBlock),
                        "Expected `{` after if condition"
                    );
                    let body = self.parse_scope();
                    push_to_statements(ASTStatement(
                        InASTStatement::If {
                            condition: cond,
                            body,
                        },
                        place,
                    ));
                }
                // While statement
                Token::Keyword(Keyword::While) => {
                    self.eat();
                    let cond = self.parse_expr();
                    eat_require!(
                        self,
                        Token::Keyword(Keyword::StartBlock),
                        "Expected `{` after while condition"
                    );
                    let body = self.parse_scope();
                    push_to_statements(ASTStatement(
                        InASTStatement::While {
                            condition: ASTBox::new(cond),
                            body,
                        },
                        place,
                    ));
                }
                Token::Keyword(Keyword::EndBlock) => {
                    self.eat();
                    return statements;
                }
                // Function call, or just expression
                &Token::Identifier(_) => {
                    // parse the expression. e.g. f(x) + 42
                    let stat = ASTStatement(InASTStatement::EvalExpr(self.parse_expr()), place);
                    // should be semicolon after expression:
                    eat_require!(
                        self,
                        Token::Keyword(Keyword::EndStatement),
                        "must end statement!"
                    );
                    push_to_statements(stat);
                }

                // return statement that returns from function with value
                Token::Keyword(Keyword::Return) => {
                    self.eat();
                    let what_to_return = self.parse_expr();
                    eat_require!(
                        self,
                        Token::Keyword(Keyword::EndStatement),
                        "Expected `;` after return expression"
                    );
                    push_to_statements(ASTStatement(InASTStatement::Return(what_to_return), place));
                }
                Token::Keyword(Keyword::StartBlock) => {
                    self.eat();
                    let scope_body = self.parse_scope();
                    push_to_statements(ASTStatement(InASTStatement::Block(scope_body), place));
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
    fn parse_primary(&mut self) -> ASTExpr {
        let (place, token) = self.eat().unwrap();
        let in_astexpr = match token {
            &Token::String(_) => self.report_incorrect_semantics(
                "Strings not allowed in language currently!",
                None,
                place,
            ),
            &Token::Int(v) => InASTExpr::Int(v),
            Token::Keyword(Keyword::StartParen) => {
                let inner_expr = self.parse_expr();
                self.eat(); // eat the end parenthesis
                return inner_expr; // forget about paren token place
            }
            &Token::Identifier(e) => {
                // can either be a variable or a function call
                match self.peek().unwrap() {
                    (_, Token::Keyword(Keyword::StartParen)) => {
                        self.eat(); // eat start paren
                        InASTExpr::FunctionCall(e, self.parse_function_call())
                    }
                    _ => InASTExpr::VarName(e),
                }
            }
            &t @ Token::Keyword(_) => {
                self.report_incorrect_semantics("no keywords as primary in expr", Some(&t), place)
            }
        };
        ASTExpr(in_astexpr, place)
    }

    /// should be called when the first `(` is eaten. Will eat the last `)`
    fn parse_function_call(&mut self) -> Vec<ASTExpr> {
        let mut arg_vec = Vec::new();
        loop {
            // eat all arguments
            let arg = self.parse_expr();
            arg_vec.push(arg);

            match self.peek().unwrap() {
                (_, Token::Keyword(Keyword::EndParen)) => {
                    break;
                }
                (_, Token::Keyword(Keyword::Comma)) => (),
                (n, &t) => self.report_incorrect_semantics("expected comma", Some(&t), n),
            }
            self.eat(); // eat comma
        }
        self.eat(); // eat end paren
        arg_vec
    }

    /// doesn't eat end of expression
    fn parse_expr(&mut self) -> ASTExpr {
        // TODO: add prefix unary operator parsing here
        let left = self.parse_primary();

        return self.parse_binop_and_right(0, left);
    }

    fn parse_binop_and_right(&mut self, prev_prec: BinOpPrecedence, mut left: ASTExpr) -> ASTExpr {
        loop {
            let (binop, our_prec, binop_place) = self.parse_oper();
            // if find binop that is LESS binding, have to go back and create expr for this like going from  * to +
            if our_prec < prev_prec {
                return left; // also, if reached end -> prec will be -1, so goes back
            }
            self.eat(); // eat operator

            // get right hand side
            let mut right = self.parse_primary();
            // peek at operator after right hand side
            let (_, next_prec, _) = self.parse_oper();
            // now we have (a+b), though if the precedence AFTER binds tighter, this is actually a + (b * ...)
            if next_prec > our_prec {
                right = self.parse_binop_and_right(our_prec + 1, right);
            }
            // merge left and right
            left = ASTExpr(
                InASTExpr::Binary(binop, ASTBox::new(left), ASTBox::new(right)),
                binop_place,
            );
        }
    }

    /// returns BinaryOp, it's precedence and it's token index.
    /// DOES NOT EAT TOKEN
    /// if ` ; ) } ` ... returns None
    fn parse_oper(&mut self) -> (BinaryOp, BinOpPrecedence, usize) {
        let (res_place, &res_token) = self.peek().unwrap();
        use Token::Keyword as T;
        let op: BinaryOp = match res_token {
            T(Keyword::Plus) => BinaryOp::Add,
            T(Keyword::Minus) => BinaryOp::Sub,
            T(Keyword::Multiply) => BinaryOp::Multiply,
            T(Keyword::Less) => BinaryOp::Less,
            T(Keyword::More) => BinaryOp::Greater,
            T(Keyword::Equals) => BinaryOp::Equals,
            T(Keyword::Set(SetType::Add)) => BinaryOp::SetAdd,
            T(Keyword::Set(SetType::Sub)) => BinaryOp::SetSub,
            T(Keyword::Set(SetType::Set)) => BinaryOp::Set,
            T(Keyword::EndParen)
            | T(Keyword::EndStatement)
            | T(Keyword::StartBlock)
            | T(Keyword::Comma) => return (BinaryOp::Add, -1, 0), // -1 precedence, end of expr

            _ => self.report_incorrect_semantics(
                &format!(
                    "What are you doing? Can't just put token `{:?}` here!",
                    res_token
                ),
                Some(&res_token),
                res_place,
            ),
        };
        (op, op.precedence(), res_place)
    }
}
