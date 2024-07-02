use std::collections::HashMap;

use crate::lexer::{IntIdx, Token};
use crate::IdentIdx;
use crate::Keyword;
use crate::SetType;

/// Binary operators with precedence as integer representation
/// IMPORTANT: WHEN ADDING NEW BINOPS: CHANGE THE "from_number" FUNCTION!
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd)]
pub enum BinaryOp {
    // numbers specify their precedence. Higher is higher precedence
    // ZERO is RESERVED for algorithm, also -1 is reserved for no more operators
    SetAdd = 1,
    SetSub = 2,
    Set = 3,
    Eql = 4,
    Les = 5,
    Mor = 6,
    Add = 7,
    Sub = 8,
    Mul = 9,
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
/// Semantic Token
/// TODO: it has aligment 2, therefore we can move the `IdentIdx` to be Soken after
/// this will bring down size from 6 to 2
/// especially FuncCall is chonky
#[derive(Debug, Copy, Clone)]
pub enum Soken {
    /// used in ast verifyer to delete Soken's in constant propogation. Should not be used or referenced otherwise
    /// can be ignored in this file anyway
    Nil,
    // (;)  we have a value, but don't use it
    EndStat,

    /// one
    Return,
    /// standalone, index into int_storage
    Int(IntIdx),
    /// R-value variable, will only be read to
    /// (parser will generate only RVar, but verifyer will convert stuff to LVar)
    RVar(IdentIdx),
    /// L-value variable, will be created by verifyer
    LVar(IdentIdx),
    /// prefixed by expression saying what to initialize the variable to, bool says whether it it is mutable or not (TRUE = MUTABLE)
    InitVar(IdentIdx, bool),
    /// Name, lookup in hashmap how many args
    FuncDef(IdentIdx),
    /// signals that function definition ended
    EndFuncDef(IdentIdx),
    /// two
    Binop(BinaryOp),
    /// Name and nr args
    /// expects nr of args
    FuncCall(IdentIdx, u16),
    /// expects three
    If,
    /// expects two
    While,
    /// {, start basic boring scope
    StartScope,
    /// }, stops the current scope by removing it's variables
    /// drop scope, not used by any later things
    EndScope,
    /// end of normal basic scope {...}, signals e.g. that if this one returns the outer one will too
    DropScope,
}

struct Parser<'a> {
    tokens: &'a [Token],
    /// token index
    ti: usize,

    // name and nr of args, later used in veryfying that called functions exist, without doing extra pass
    functions: HashMap<IdentIdx, u16>,
    // semantic tokens
    sokens: Vec<Soken>,
    // parallel array of index of Token we came from
    origins: Vec<usize>,

    // immutable:
    token_idx_to_char_range: &'a Vec<(usize, usize)>,
    source: &'a str,
    file_name: &'a str,
}

/// returns Abstract Syntax Tree with Function hashmap which contains all functions declarations
pub fn parse(
    tokens: &[Token],
    token_idx_to_char_range: &Vec<(usize, usize)>,
    source: &str,
    file_name: &str,
) -> (Vec<Soken>, Vec<usize>, HashMap<IdentIdx, u16>) {
    let mut parser = Parser {
        tokens,
        ti: 0,
        functions: HashMap::new(),
        sokens: Vec::new(),
        origins: Vec::new(),

        token_idx_to_char_range,
        source,
        file_name,
    };

    parser.parse_scope();

    (parser.sokens, parser.origins, parser.functions)
}
/// takes self, pattern and error message
macro_rules! eat_require {
    ($self:ident, $the_pattern:pat, $msg:literal) => {
        match { $self.eat() } {
            ($the_pattern, _) => (),
            _ => $self.report($msg),
        }
    };
}

impl<'parser_lifetime> Parser<'parser_lifetime> {
    fn eat(&mut self) -> (Token, usize) {
        let ret = (self.tokens[self.ti], self.ti);
        println!("eat token {:?}", ret.0);
        self.ti += 1;
        ret
    }
    fn peek(&mut self) -> (Token, usize) {
        (self.tokens[self.ti], self.ti)
    }
    /// reports the NEXT token (call this when you peeked)
    fn report_n(&mut self, msg: &str) -> ! {
        self.eat();
        self.report(msg);
    }

    /// reports the current token, which was previously eaten
    fn report(&mut self, msg: &str) -> ! {
        let token_start_idx = self.ti - 1;
        let bad_tok = self.tokens[token_start_idx];
        crate::mark_error_in_source(
            self.source,
            self.file_name,
            msg,
            self.token_idx_to_char_range[token_start_idx],
        );
        println!("\x1b[0m    bad token: `{:?}`", bad_tok);

        std::process::exit(1);
    }

    /// require {
    fn new_scope(&mut self, add_scope_start_soken: bool) {
        eat_require!(
            self,
            Token::Keyword(Keyword::StartBlock),
            "Expected `{` at start of scope"
        );
        if add_scope_start_soken {
            self.pu(Soken::StartScope);
        }
        self.parse_scope();
    }
    // expects semicolon and pushes EndStat Soken
    fn require_semicolon(&mut self, push: bool) {
        eat_require!(
            self,
            Token::Keyword(Keyword::EndStatement),
            "statement must end with `;`"
        );
        if push {
            self.pu(Soken::EndStat);
        }
    }
    /// add new semantic token, which was found at place
    fn push(&mut self, soken: Soken, place: usize) {
        self.sokens.push(soken);
        self.origins.push(place);
    }
    /// add new semantic token, which was found at current .eat()
    fn pu(&mut self, soken: Soken) {
        self.sokens.push(soken);
        self.origins.push(self.ti - 1);
    }

    /// Parse generalized tokens. Probably statements. If finds }, eats. Also ends at end of tokens
    /// Variable name => Set it to expression, If => generate If node
    /// eats and outputs } soken
    fn parse_scope(&mut self) {
        while self.ti < self.tokens.len() {
            let (token, place) = self.eat();
            use Keyword as K;
            use Token as T;
            match token {
                T::Keyword(K::FunctionIncoming) => {
                    // Next token should be function name identifier
                    let func_name = {
                        let func_name = match self.eat() {
                            (T::Identifier(e), _) => e,
                            _ => self.report("Expected function name identifier"),
                        };
                        if self.functions.contains_key(&func_name) {
                            self.report("Function with same name already declared!")
                        }
                        func_name
                    };
                    self.pu(Soken::FuncDef(func_name));
                    // Next token should be start paren
                    eat_require!(
                        self,
                        T::Keyword(K::StartParen),
                        "Expected start parenthesis after function name"
                    );
                    // Next should be a series of name and type-names, ending with a end paren
                    let mut args = 0;
                    loop {
                        let (arg, _) = self.eat();
                        let arg_name = match arg {
                            T::Identifier(arg_name) => arg_name,
                            T::Keyword(K::EndParen) => break,
                            _ => self.report("Expected argument name"),
                        };
                        args += 1;
                        self.pu(Soken::RVar(arg_name));
                        // Next should be a comma or end paren
                        match self.eat() {
                            (T::Keyword(K::Comma), _) => (),
                            (T::Keyword(K::EndParen), _) => break,
                            _ => self.report("Expected comma or end parenthesis after argument"),
                        };
                    }
                    self.functions.insert(func_name, args);
                    self.new_scope(false);
                    self.pu(Soken::EndFuncDef(func_name));
                }
                // create variable with `let`, just lookahead on name, later code will handle rest as expression
                // TODO fix so you can't do let a += 1; or let b + 3;
                T::Keyword(K::CreateConstVar) | T::Keyword(K::CreateMutVar) => {
                    let mutable = match token {
                        T::Keyword(K::CreateConstVar) => false,
                        T::Keyword(K::CreateMutVar) => true,
                        _ => unreachable!(),
                    };
                    // Next token should be variable-name
                    let maybe_name = self.eat();
                    let name = match maybe_name.0 {
                        T::Identifier(e) => e,
                        _ => self.report("Expected variable name"),
                    };
                    // Next token should be =
                    eat_require!(
                        self,
                        T::Keyword(K::Set(SetType::Set)),
                        "Require = after declaring variable"
                    );
                    // parse expression let (a = 1+2+3)
                    self.parse_expr(None); // this outputs expr
                    self.push(Soken::InitVar(name, mutable), place);
                    self.require_semicolon(true);
                }
                // If statement
                T::Keyword(K::If) => {
                    self.parse_expr(None);
                    self.new_scope(true);
                    let (possible_else, _) = self.peek();
                    if matches!(possible_else, T::Keyword(K::Else)) {
                        self.eat();
                        self.new_scope(true);
                    } else {
                        self.pu(Soken::StartScope);
                        self.pu(Soken::EndScope);
                    }
                    self.push(Soken::If, place);
                }
                // While statement
                T::Keyword(K::While) => {
                    self.parse_expr(None);
                    self.new_scope(true);
                    self.push(Soken::While, place);
                }
                // return statement that returns from function with value
                T::Keyword(K::Return) => {
                    self.parse_expr(None);
                    self.push(Soken::Return, place);
                    self.require_semicolon(false);
                }
                // basic scope has to drop scope object after itself
                T::Keyword(K::StartBlock) => {
                    self.pu(Soken::StartScope);
                    self.parse_scope();
                    self.pu(Soken::DropScope);
                }
                T::Keyword(K::EndBlock) => {
                    self.pu(Soken::EndScope);
                    return;
                }
                // Function call, or just expression
                T::Identifier(_) | T::Int(_) | T::String(_) => {
                    // parse the expression. e.g. f(x) + 42, or 32 + g(f(x))
                    self.parse_expr(Some((token, place)));
                    self.require_semicolon(true);
                }
                _ => self.report("Erroneous token to start statement with"),
            };
        }
    }

    /// doesn't eat end of expression, so you can check if the expr ended with ; or {
    fn parse_expr(&mut self, first: Option<(Token, usize)>) {
        // TODO: add prefix unary operator parsing here
        let b = first.unwrap_or_else(|| self.eat());

        self.parse_primary(b.0, b.1);

        self.parse_binop_and_right(0)
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
            {
                let a = self.eat();
                self.parse_primary(a.0, a.1);
            }
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

    fn parse_primary(&mut self, token: Token, p: usize) {
        let soken = match token {
            Token::String(_) => self.report("Strings not allowed in language currently!"),
            Token::Int(e) => Soken::Int(e),
            Token::Keyword(Keyword::StartParen) => {
                self.parse_expr(None);
                self.eat(); // eat the end parenthesis
                return;
            }
            Token::Identifier(e) => {
                // can either be a variable or a function call
                match self.peek() {
                    (Token::Keyword(Keyword::StartParen), _) => {
                        self.eat(); // eat start paren
                        let args = self.parse_function_call_args();
                        Soken::FuncCall(e, args)
                    }
                    _ => Soken::RVar(e),
                }
            }
            Token::Keyword(_) => self.report("no keywords as primary in expr"),
        };
        self.push(soken, p);
    }

    /// should be called when the first `(` is eaten. Will eat the last `)`
    /// returns nr of args function was called with
    fn parse_function_call_args(&mut self) -> u16 {
        let mut args = 0;
        // if no args
        if let (Token::Keyword(Keyword::EndParen), _) = self.peek() {
        } else {
            // eat all arguments
            loop {
                self.parse_expr(None);
                args += 1;

                match self.peek() {
                    (Token::Keyword(Keyword::EndParen), _) => break,
                    (Token::Keyword(Keyword::Comma), _) => {
                        self.eat(); // eat comma
                    }
                    _ => self.report_n("expected comma"),
                }
            }
        }
        self.eat(); // eat end paren
        args
    }

    /// returns precedence of binop and it's token index.
    /// DOES NOT EAT TOKEN
    /// if ` ; ) } ` ... returns -1 (special precedence)
    fn parse_oper(&mut self) -> (BinOpPrec, usize) {
        let (res_token, res_place) = self.peek();
        use BinaryOp as B;
        use Keyword as K;
        use Token::Keyword as T;
        let prec = match res_token {
            T(K::Plus) => B::Add.prec(),
            T(K::Minus) => B::Sub.prec(),
            T(K::Multiply) => B::Mul.prec(),
            T(K::Less) => B::Les.prec(),
            T(K::More) => B::Mor.prec(),
            T(K::Equals) => B::Eql.prec(),
            T(K::Set(SetType::Add)) => B::SetAdd.prec(),
            T(K::Set(SetType::Sub)) => B::SetSub.prec(),
            T(K::Set(SetType::Set)) => B::Set.prec(),
            T(K::EndParen) | T(K::EndStatement) | T(K::StartBlock) | T(K::Comma) => -1, // -1 precedence, end of expr
            _ => self.report_n(&format!(
                "What are you doing? Can't just put token `{:?}` here!",
                res_token
            )),
        };
        (prec, res_place)
    }
}
