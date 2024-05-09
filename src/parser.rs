use crate::lexer::Token;
use crate::IdentifierIdx;
use crate::Int;
use crate::Keyword;
use crate::SetType;

type ASTBox<T> = Box<T>;
pub type ASTBody = Vec<ASTStatement>;

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum BinaryOp {
    // numbers specify their precedence. Higher means it should be evaluated first
    // ZERO is RESERVED for algorithm!!!!!!!!
    SetAdd = 1,
    SetSub = 2,
    Set = 3,
    Equals = 4,
    Less = 5,
    More = 6,
    Add = 7,
    Sub = 8,
    Multiply = 9,
}

type BinOpPrecedence = u8;

impl BinaryOp {
    fn precedence(self) -> BinOpPrecedence {
        self as _
    }
}

/// holds InAstExpr and token idx of expr
#[derive(Debug)]
pub struct ASTExpr (pub InASTExpr, pub usize);

#[derive(Debug)]
pub enum InASTExpr {
    Int(Int),
    String(&'static str),
    VarName(IdentifierIdx),
    FunctionCall(IdentifierIdx, Vec<ASTExpr>), // a function and its arguments
    Binary(BinaryOp, ASTBox<ASTExpr>, ASTBox<ASTExpr>),
}

#[derive(Debug)]
pub enum ASTStatement {
    If {
        condition: ASTBox<ASTExpr>,
        body: ASTBody,
    },
    While {
        condition: ASTBox<ASTExpr>,
        body: ASTBody,
    },
    EvalExpr(ASTExpr), // for just evaulating an expression like a function call
    CreateVar(IdentifierIdx),
    Function {
        name: IdentifierIdx,
        args: Vec<IdentifierIdx>, // name
        body: ASTBody,
    },
}

struct Parser<'parser_lifetime> {
    tokens: &'parser_lifetime mut std::iter::Peekable<
        std::iter::Enumerate<std::slice::Iter<'parser_lifetime, Token>>,
    >,
    token_idx_to_char_range: &'parser_lifetime Vec<(usize, usize)>,

    source: &'parser_lifetime str,
    file_name: &'parser_lifetime str,
}

pub fn parse(
    tokens: &[Token],
    token_idx_to_char_range: &Vec<(usize, usize)>,
    source: &str,
    file_name: &str,
) -> ASTBody {
    let mut token_iter = tokens.iter().enumerate().peekable();
    let mut parser = Parser::new(&mut token_iter, token_idx_to_char_range, source, file_name);
    parser.parse_scope()
}
enum InnerReturn {
    ASTThing(ASTExpr),
    /// operation, expression, token index of BinaryOp
    GoBack(BinaryOp, ASTExpr, usize),
}

impl<'parser_lifetime> Parser<'parser_lifetime> {
    fn new(
        tokens: &'parser_lifetime mut std::iter::Peekable<
            std::iter::Enumerate<std::slice::Iter<'parser_lifetime, Token>>,
        >,
        token_idx_to_char_range: &'parser_lifetime Vec<(usize, usize)>,
        source: &'parser_lifetime str,
        file_name: &'parser_lifetime str,
    ) -> Self {
        Parser {
            tokens,
            token_idx_to_char_range,
            source,
            file_name,
        }
    }
    fn eat(&mut self) -> Option<(usize, &Token)> {
        self.tokens.next()
    }

    fn peek(&mut self) -> Option<(usize, &Token)> {
        self.tokens.peek().copied()
    }

    fn report_incorrect_semantics(
        // TODO, better error reporting
        &mut self,
        msg: &str,
        _bad_tok: &Token,
        token_start_pos: usize,
    ) -> ! {
        let (start_wher, _end_wher) = self.token_idx_to_char_range[token_start_pos];

        let mut line = 1;
        let mut col = 1;
        let mut latest_start_of_newline = 0;
        for (idx, ch) in self.source.chars().enumerate() {
            if idx == start_wher {
                break;
            }
            if ch == '\n' {
                line += 1;
                col = 1;
                latest_start_of_newline = idx + 1;
            } else {
                col += 1;
            }
        }
        // get string for line by iterating over chars again
        let line_str = {
            let mut end_of_line = 0;
            for (idx, ch) in self.source.chars().enumerate() {
                if idx > latest_start_of_newline && ch == '\n' {
                    // found end of line
                    end_of_line = idx;
                    break;
                }
            }
            &self.source[latest_start_of_newline..end_of_line]
        };

        println!("\n --- ERROR IN PARSING ---");
        println!("\x1b[1m\x1b[31merror:\x1b[39m {}", msg);
        println!("\x1b[34m  --> \x1b[0m {}:{}:{}", self.file_name, line, col);
        let start_string = format!("{} |", line);
        for _ in 0..start_string.len() - 1 {
            print!(" ");
        }
        println!("\x1b[1m\x1b[34m|");
        println!("{}\x1b[0m{}", start_string, line_str);
        // print arrows pointing to were the error is

        for at in 0..(col - 1 + start_string.len()) {
            print!(
                "{}",
                match at {
                    a if a == start_string.len() - 1 => "\x1b[1m\x1b[34m|\x1b[0m",
                    _ => " ",
                }
            );
        }
        print!("\x1b[1m\x1b[31m");
        for _ in 0..(_end_wher - start_wher) {
            print!("^");
        }
        println!();
        println!("\x1b[0m    bad token: `{:?}`", _bad_tok);

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
            match match self.peek() {
                Some(tok) => {
                    println!("token {:?}", tok);
                    tok
                }
                None => break,
            } {
                (_, Token::Keyword(Keyword::FunctionIncoming)) => {
                    self.eat();
                    // Next token should be function name identifier
                    let func_name = match self.eat().unwrap() {
                        (_, &Token::Identifier(func_name)) => func_name,
                        (tok_nr, &tok) => self.report_incorrect_semantics(
                            "Expected function name identifier",
                            &tok,
                            tok_nr,
                        ),
                    };
                    // Next token should be start paren
                    match self.eat().unwrap() {
                        (_, Token::Keyword(Keyword::StartParen)) => (),
                        (tok_nr, &tok) => self.report_incorrect_semantics(
                            "Expected start parenthesis after function name",
                            &tok,
                            tok_nr,
                        ),
                    };
                    // Next should be a series of name and type-names, ending with a end paren
                    let mut arg_vec = Vec::new();
                    loop {
                        let arg_name = match self.eat().unwrap() {
                            (_, &Token::Identifier(arg_name)) => arg_name,
                            (_, Token::Keyword(Keyword::EndParen)) => break,
                            (tok_nr, &tok) => self.report_incorrect_semantics(
                                "Expected argument name",
                                &tok,
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
                                &t,
                                n,
                            ),
                        };
                    }
                    // must be { after this
                    match self.eat().unwrap() {
                        (_, Token::Keyword(Keyword::StartBlock)) => (),
                        (n, &t) => self.report_incorrect_semantics(
                            "Expected `{` after function declaration",
                            &t,
                            n,
                        ),
                    };
                    push_to_statements(ASTStatement::Function {
                        name: func_name,
                        args: arg_vec,
                        body: self.parse_scope(),
                    });
                }
                // create variable with `let`
                (_, &Token::Keyword(Keyword::CreateVar)) => {
                    self.eat();
                    // Next token should be variable-name
                    let (var_idx, var_token_place) = match self.eat().unwrap() {
                        (p, &Token::Identifier(var_idx)) => (var_idx, p),
                        (n, &t) => self.report_incorrect_semantics("expected variable name", &t, n),
                    };
                    // make sure there is a SET token after var name
                    let set_token_place = match self.eat().unwrap() {
                        (p, Token::Keyword(Keyword::Set(SetType::Set))) => p,
                        (n, &t) => self.report_incorrect_semantics("expected set token", &t, n),
                    };
                    // now there should be an expression
                    let expr = ASTBox::new(self.parse_expr());
                    // it should end with a semicolon
                    match self.eat().unwrap() {
                        (_, Token::Keyword(Keyword::EndStatement)) => (),
                        (n, &t) => self.report_incorrect_semantics(
                            "expected `;` after variable declaration",
                            &t,
                            n,
                        ),
                    };
                    push_to_statements(ASTStatement::CreateVar(var_idx)); // create variable
                    push_to_statements(ASTStatement::EvalExpr(ASTExpr(InASTExpr::Binary(
                        BinaryOp::Set,
                        ASTBox::new(ASTExpr(InASTExpr::VarName(var_idx), var_token_place)),
                        expr,
                    ), set_token_place)));
                }
                // If statement
                (_, Token::Keyword(Keyword::If)) => {
                    self.eat();
                    let cond = self.parse_expr();
                    match self.eat().unwrap() {
                        (_, Token::Keyword(Keyword::StartBlock)) => (),
                        (n, &t) => self.report_incorrect_semantics(
                            "Expected `{` after if condition",
                            &t,
                            n,
                        ),
                    };
                    let body = self.parse_scope();
                    push_to_statements(ASTStatement::If {
                        condition: ASTBox::new(cond),
                        body,
                    });
                }
                // While statement
                (_, Token::Keyword(Keyword::While)) => {
                    self.eat();
                    let cond = self.parse_expr();
                    match self.eat().unwrap() {
                        (_, Token::Keyword(Keyword::StartBlock)) => (),
                        (n, &t) => self.report_incorrect_semantics(
                            "Expected `{` after while condition",
                            &t,
                            n,
                        ),
                    };
                    let body = self.parse_scope();
                    push_to_statements(ASTStatement::While {
                        condition: ASTBox::new(cond),
                        body,
                    });
                }
                (_, Token::Keyword(Keyword::EndBlock)) => {
                    self.eat();
                    return statements;
                }
                // Function call, or just expression
                (_, &Token::Identifier(e)) => {
                    // make sure there is a start paren after function name
                    let stat = ASTStatement::EvalExpr(self.parse_expr());
                    // should be semicolon after expression:
                    match self.peek().unwrap() {
                        (_, Token::Keyword(Keyword::EndStatement)) => {
                            self.eat();
                        }
                        (start, &t) => {
                            self.report_incorrect_semantics("must end statement!", &t, start)
                        }
                    }
                    push_to_statements(stat);

                    /* 
                    match self.eat().unwrap() {
                        // function call
                        (_, Token::Keyword(Keyword::StartParen)) => {  
                            let func_args = self.parse_function_call();
                            // make sure it ends with a semicolon
                            match self.eat().unwrap() {
                                (_, Token::Keyword(Keyword::EndStatement)) => (),
                                (n, &t) => self.report_incorrect_semantics(
                                    "expected end statement after function call",
                                    &t,
                                    n,
                                ),
                            };
                            push_to_statements(ASTStatement::EvalExpr(ASTExpr::FunctionCall(e, func_args)));
                        },

                        (n, &t) => self.report_incorrect_semantics(
                            "expected start parenthesis after identifier in statement",
                            &t,
                            n,
                        ),
                    };
                    */
                }

                (n, &t) => self.report_incorrect_semantics(
                    "Erroneous token to start statement with",
                    &t,
                    n,
                ),
            };
        }
        statements
    }

    /// should be called when the first `(` is eaten, eats the last `)`
    fn parse_function_call(&mut self) -> Vec<ASTExpr> {
        let mut arg_vec = Vec::new();
        while match self.peek().unwrap() {
            (_, Token::Keyword(Keyword::EndParen)) => false,
            (_, Token::Keyword(Keyword::Comma)) => {
                self.eat();
                true
            }
            _ => true,
            //  (n, &t) => self.report_incorrect_semantics("expected comma", &t, n),
        } {
            // eat all arguments
            let arg = self.parse_expr();
            arg_vec.push(arg);
        }
        self.eat(); // eat end paren
        arg_vec
    }

    // doesn't eat end of expression
    fn parse_expr(&mut self) -> ASTExpr {
        match self.parse_inner(0) {
            InnerReturn::ASTThing(expr) => expr,
            InnerReturn::GoBack(_, _, _) => panic!("Can't go back in highest level???"),
        }
    }

    fn parse_leaf(&mut self) -> ASTExpr {
        let (place, token) = self.eat().unwrap();
        let in_astexpr = match token {
            Token::String(string) => InASTExpr::String(string),
            &Token::Int(v) => InASTExpr::Int(v),
             Token::Keyword(Keyword::StartParen) => {
                // create new parse scope here
                match self.parse_inner(0) {
                    InnerReturn::ASTThing(e) => {
                        self.eat();
                        return e
                    } // eat the end parenthesis
                    _ => panic!("can't go back in parenthesis highest level???"),
                }
            }
            &Token::Identifier(e) => {
                // can either be a variable or a function call
                match self.peek().unwrap() {
                    (_, Token::Keyword(Keyword::StartParen)) => {
                        // function call
                        self.eat(); // eat start paren
                        InASTExpr::FunctionCall(e, self.parse_function_call())
                    }
                    _ => InASTExpr::VarName(e),
                }
            }
             &t @ Token::Keyword(_) => {
                self.report_incorrect_semantics("no keywords as leaf in expr", &t, place)
            }
        };
        ASTExpr(in_astexpr, place)
    }

    fn parse_inner(&mut self, prev_precedence: BinOpPrecedence) -> InnerReturn {
        let value = self.parse_leaf();

        let (binop, binop_place) = match self.parse_oper() {
            Some(binop) => binop,
            None => {
                // no operator, return value
                return InnerReturn::ASTThing(value);
            }
        };
        self.eat(); // eat operator
        let precedence = binop.precedence();
        if precedence < prev_precedence {
            // we have to back and collect things in this "precedence block"
            return InnerReturn::GoBack(binop, value, binop_place);
        }

        let mut our_first_part = ASTBox::new(value);
        let mut our_binop = binop;
        let mut our_binop_place = binop_place;

        loop {
            match self.parse_inner(precedence) {
                InnerReturn::ASTThing(e) => {
                    return InnerReturn::ASTThing(ASTExpr(InASTExpr::Binary(
                        our_binop,
                        our_first_part,
                        ASTBox::new(e),
                    ), our_binop_place));
                }
                InnerReturn::GoBack(lower_binop, expr, lower_binop_place) => {
                    if lower_binop.precedence() >= prev_precedence {
                        // combine, but we can continue. Contain our gotten expression into the new
                        // operator will will contain us (fine because previous precedence was
                        // similar)
                        our_first_part = ASTBox::new(ASTExpr(InASTExpr::Binary(
                            our_binop,
                            our_first_part,
                            ASTBox::new(expr),
                        ), our_binop_place));
                        our_binop = lower_binop;
                        our_binop_place = lower_binop_place;
                    } else {
                        // too low precedence, must go back
                        return InnerReturn::GoBack(
                            lower_binop,
                            ASTExpr(InASTExpr::Binary(our_binop, our_first_part, ASTBox::new(expr)), our_binop_place),
                            lower_binop_place,
                        );
                    }
                }
            }
        }
    }

    /// returns possible BinaryOp and it's token index.
    /// DOES NOT EAT TOKEN
    fn parse_oper(&mut self) -> Option<(BinaryOp, usize)> {
        let (res_place, &res_token) = self.peek().unwrap();
        let op: BinaryOp = match res_token {
            Token::Keyword(keyword) => match keyword {
                Keyword::Plus => BinaryOp::Add,
                Keyword::Minus => BinaryOp::Sub,
                Keyword::Multiply => BinaryOp::Multiply,
                Keyword::Less => BinaryOp::Less,
                Keyword::More => BinaryOp::More,
                Keyword::Equals => BinaryOp::Equals,
                Keyword::Set(SetType::Add) => BinaryOp::SetAdd,
                Keyword::Set(SetType::Sub) => BinaryOp::SetSub,
                Keyword::Set(SetType::Set) => BinaryOp::Set,
                Keyword::EndParen
                | Keyword::EndStatement
                | Keyword::StartBlock
                | Keyword::Comma => return None,
                _ => self.report_incorrect_semantics(
                    "Token not a binary operation",
                    &res_token,
                    res_place,
                ),
            },
            _ => return None,
        };
        Some ((op, res_place))
    }
}

pub fn print_ast(body: &ASTBody) {
    fn print_indent(indent: u64) {
        for _ in 0..(indent * 2) {
            print!(" ");
        }
    }

    fn print_expr(expr: &ASTExpr, indent: u64) {
        print_indent(indent);
        match &expr.0 {
            InASTExpr::Int(num) => {
                println!("{:?}", num);
            }
            InASTExpr::String(st) => {
                println!("{:?}", st);
            }
            InASTExpr::VarName(var_idx) => {
                println!("var: {}", var_idx);
            }
            InASTExpr::FunctionCall(name, args) => {
                println!("func: {}", name);
                println!("args: {:?}", args);
            }
            InASTExpr::Binary(op, f, s) => {
                println!("op: {:?}", op);
                print_expr(f, indent + 1);
                print_expr(s, indent + 1);
            } // ASTExpr::Neg(expr) => {
              //     println!("neg of:");
              //     print_expr(expr, indent + 1);
              // }
        }
    }

    fn print_body(body: &ASTBody, indent: u64) {
        print_indent(indent);
        for statement in body {
            match statement {
                ASTStatement::If { condition, body } => {
                    println!("If:");
                    print_expr(condition, indent + 1);
                    print_indent(indent);
                    println!("Then:");
                    print_body(body, indent + 1);
                }
                ASTStatement::While { condition, body } => {
                    println!("While:");
                    print_expr(condition, indent + 1);
                    print_indent(indent);
                    println!("Do:");
                    print_body(body, indent + 1);
                }
                ASTStatement::EvalExpr(expr) => {
                    println!("Eval following:");
                    print_expr(expr, indent + 1);
                }
                ASTStatement::Function { name, args, body } => {
                    println!("Function: {}", name);
                    println!("Args: {:?}", args);
                    print_indent(indent);
                    println!("Body:");
                    print_body(body, indent + 1);
                }
                ASTStatement::CreateVar(var_name) => {
                    println!("Create variable: {}", var_name);
                }
            }
        }
    }

    print_body(body, 0);
}
