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
    // numbers specify their precedence. Higher means it should be evaluated first
    // ZERO is RESERVED for algorithm!!!!!!!!
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

type BinOpPrecedence = u8;

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
        condition: ASTBox<ASTExpr>,
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
enum InnerReturn {
    ASTThing(ASTExpr),
    /// operation, expression, token index of BinaryOp
    GoBack(BinaryOp, ASTExpr, usize),
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
            match match self.peek() {
                Some(tok) => {
                    println!("token {:?}", tok);
                    tok
                }
                None => break,
            } {
                (function_incoming_token, Token::Keyword(Keyword::FunctionIncoming)) => {
                    self.eat();
                    // Next token should be function name identifier
                    let (func_name_token_position, func_name) = match self.eat().unwrap() {
                        (p, &Token::Identifier(func_name)) => (p, func_name),
                        (tok_nr, &tok) => self.report_incorrect_semantics(
                            "Expected function name identifier",
                            Some(&tok),
                            tok_nr,
                        ),
                    };
                    // Next token should be start paren
                    match self.eat().unwrap() {
                        (_, Token::Keyword(Keyword::StartParen)) => (),
                        (tok_nr, &tok) => self.report_incorrect_semantics(
                            "Expected start parenthesis after function name",
                            Some(&tok),
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
                    match self.eat().unwrap() {
                        (_, Token::Keyword(Keyword::StartBlock)) => (),
                        (n, &t) => self.report_incorrect_semantics(
                            "Expected `{` after function declaration",
                            Some(&t),
                            n,
                        ),
                    };

                    {
                        // add function to function hashmap
                        if self.functions.contains_key(&func_name) {
                            // functions already exists, error!
                            self.report_incorrect_semantics(
                                "Function with same name already declared!",
                                None,
                                func_name_token_position,
                            )
                        }
                        self.functions.insert(func_name, arg_vec.len() as _);
                    }
                    push_to_statements(ASTStatement(
                        InASTStatement::Function {
                            name: func_name,
                            args: arg_vec,
                            body: self.parse_scope(),
                        },
                        function_incoming_token,
                    ));
                }
                // create variable with `let`
                (create_var_token, &Token::Keyword(Keyword::CreateVar)) => {
                    self.eat();
                    // Next token should be variable-name
                    let (var_idx, var_token_place) = match self.eat().unwrap() {
                        (p, &Token::Identifier(var_idx)) => (var_idx, p),
                        (n, &t) => {
                            self.report_incorrect_semantics("expected variable name", Some(&t), n)
                        }
                    };
                    // make sure there is a SET token after var name
                    let set_token_place = match self.eat().unwrap() {
                        (p, Token::Keyword(Keyword::Set(SetType::Set))) => p,
                        (n, &t) => {
                            self.report_incorrect_semantics("expected set token", Some(&t), n)
                        }
                    };
                    // now there should be an expression
                    let expr = ASTBox::new(self.parse_expr());
                    // it should end with a semicolon
                    match self.eat().unwrap() {
                        (_, Token::Keyword(Keyword::EndStatement)) => (),
                        (n, &t) => self.report_incorrect_semantics(
                            "expected `;` after variable declaration",
                            Some(&t),
                            n,
                        ),
                    };
                    push_to_statements(ASTStatement(
                        InASTStatement::CreateVar(var_idx),
                        create_var_token,
                    )); // create variable

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
                (if_token_place, Token::Keyword(Keyword::If)) => {
                    self.eat();
                    let cond = self.parse_expr();
                    match self.eat().unwrap() {
                        (_, Token::Keyword(Keyword::StartBlock)) => (),
                        (n, &t) => self.report_incorrect_semantics(
                            "Expected `{` after if condition",
                            Some(&t),
                            n,
                        ),
                    };
                    let body = self.parse_scope();
                    push_to_statements(ASTStatement(
                        InASTStatement::If {
                            condition: ASTBox::new(cond),
                            body,
                        },
                        if_token_place,
                    ));
                }
                // While statement
                (while_place, Token::Keyword(Keyword::While)) => {
                    self.eat();
                    let cond = self.parse_expr();
                    match self.eat().unwrap() {
                        (_, Token::Keyword(Keyword::StartBlock)) => (),
                        (n, &t) => self.report_incorrect_semantics(
                            "Expected `{` after while condition",
                            Some(&t),
                            n,
                        ),
                    };
                    let body = self.parse_scope();
                    push_to_statements(ASTStatement(
                        InASTStatement::While {
                            condition: ASTBox::new(cond),
                            body,
                        },
                        while_place,
                    ));
                }
                (_, Token::Keyword(Keyword::EndBlock)) => {
                    self.eat();
                    return statements;
                }
                // Function call, or just expression
                (place, &Token::Identifier(_)) => {
                    // parse the expression. e.g. f(x) + 42
                    let stat = ASTStatement(InASTStatement::EvalExpr(self.parse_expr()), place);
                    // should be semicolon after expression:
                    match self.peek().unwrap() {
                        (_, Token::Keyword(Keyword::EndStatement)) => {
                            self.eat();
                        }
                        (start, &t) => {
                            self.report_incorrect_semantics("must end statement!", Some(&t), start)
                        }
                    }
                    push_to_statements(stat);
                }

                // return statement that returns from function with value
                (place, Token::Keyword(Keyword::Return)) => {
                    self.eat();
                    let what_to_return = self.parse_expr();
                    match self.eat().unwrap() {
                        (_, Token::Keyword(Keyword::EndStatement)) => (),
                        (n, &t) => self.report_incorrect_semantics(
                            "Expected `;` after return expression!",
                            Some(&t),
                            n,
                        ),
                    };
                    push_to_statements(ASTStatement(InASTStatement::Return(what_to_return), place));
                }
                (place, Token::Keyword(Keyword::StartBlock)) => {
                    self.eat();
                    let scope_body = self.parse_scope();
                    push_to_statements(ASTStatement(InASTStatement::Block(scope_body), place));
                }

                (n, &t) => self.report_incorrect_semantics(
                    "Erroneous token to start statement with",
                    Some(&t),
                    n,
                ),
            };
        }
        statements
    }

    /// should be called when the first `(` is eaten. Will eat the last `)`
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

    /// doesn't eat end of expression
    fn parse_expr(&mut self) -> ASTExpr {
        match self.parse_inner(0) {
            InnerReturn::ASTThing(expr) => expr,
            InnerReturn::GoBack(_, _, _) => panic!("Can't go back in highest level???"),
        }
    }

    fn parse_leaf(&mut self) -> ASTExpr {
        let (place, token) = self.eat().unwrap();
        let in_astexpr = match token {
            &Token::String(_) => self.report_incorrect_semantics(
                "Strings not allowed in language currently!",
                None,
                place,
            ),
            &Token::Int(v) => InASTExpr::Int(v),
            Token::Keyword(Keyword::StartParen) => {
                // create new parse scope here
                match self.parse_inner(0) {
                    InnerReturn::ASTThing(e) => {
                        self.eat();
                        return e;
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
                self.report_incorrect_semantics("no keywords as leaf in expr", Some(&t), place)
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
                    return InnerReturn::ASTThing(ASTExpr(
                        InASTExpr::Binary(our_binop, our_first_part, ASTBox::new(e)),
                        our_binop_place,
                    ));
                }
                InnerReturn::GoBack(lower_binop, expr, lower_binop_place) => {
                    if lower_binop.precedence() >= prev_precedence {
                        // combine, but we can continue. Contain our gotten expression into the new
                        // operator will will contain us (fine because previous precedence was
                        // similar)
                        our_first_part = ASTBox::new(ASTExpr(
                            InASTExpr::Binary(our_binop, our_first_part, ASTBox::new(expr)),
                            our_binop_place,
                        ));
                        our_binop = lower_binop;
                        our_binop_place = lower_binop_place;
                    } else {
                        // too low precedence, must go back
                        return InnerReturn::GoBack(
                            lower_binop,
                            ASTExpr(
                                InASTExpr::Binary(our_binop, our_first_part, ASTBox::new(expr)),
                                our_binop_place,
                            ),
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
            | T(Keyword::Comma) => return None,
            _ => self.report_incorrect_semantics(
                &format!(
                    "What are you doing? Can't just put token `{:?}` here!",
                    res_token
                ),
                Some(&res_token),
                res_place,
            ),
        };
        Some((op, res_place))
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
            match &statement.0 {
                InASTStatement::If { condition, body } => {
                    println!("If:");
                    print_expr(condition, indent + 1);
                    print_indent(indent);
                    println!("Then:");
                    print_body(body, indent + 1);
                }
                InASTStatement::While { condition, body } => {
                    println!("While:");
                    print_expr(condition, indent + 1);
                    print_indent(indent);
                    println!("Do:");
                    print_body(body, indent + 1);
                }
                InASTStatement::EvalExpr(expr) => {
                    println!("Eval following:");
                    print_expr(expr, indent + 1);
                }
                InASTStatement::Function { name, args, body } => {
                    println!("Function: {}", name);
                    println!("Args: {:?}", args);
                    print_indent(indent);
                    println!("Body:");
                    print_body(body, indent + 1);
                }
                InASTStatement::CreateVar(var_name) => {
                    println!("Create variable: {}", var_name);
                }
                InASTStatement::Return(expr) => {
                    println!("Return: ");
                    print_expr(expr, indent + 1);
                }
                InASTStatement::Block(block) => {
                    println!("Block: ");
                    print_indent(indent);
                    println!("Body:");
                    print_body(block, indent + 1);
                }
            }
        }
    }

    print_body(body, 0);
}
