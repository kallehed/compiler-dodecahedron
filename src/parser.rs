use std::collections::HashMap;

use crate::lexer::Token;
use crate::IdentifierIdx;
use crate::Int;
use crate::Keyword;
use crate::SetType;
use crate::Type;

type ASTBox<T> = Box<T>;
pub type ASTBody = Vec<ASTStatement>;

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum BinaryOp {
    // numbers specify their precedence. Higher means it should be evaluated first
    // ZERO is RESERVED for algorithm!!!!!!!!
    SetAdd = 1,
    SetSub = 2,
    Set = 3,
    Argument = 4,
    Equals = 5,
    Less = 6,
    More = 7,
    Add = 8,
    Sub = 9,
    Multiply = 10,
}

type BinOpPrecedence = u8;

impl BinaryOp {
    fn precedence(self) -> BinOpPrecedence {
        self as _
    }
}

#[derive(Debug)]
pub enum ASTExpr {
    Int(Int),
    _String(&'static str),
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
    Function {
        name: IdentifierIdx,
        args: Vec<(IdentifierIdx, IdentifierIdx)>, // name, type
        body: ASTBody,
    },
}

pub fn check_that_ast_is_correct(body: &ASTBody) {
    fn type_of_expr(expr: &ASTExpr) -> Type {
        match expr {
            ASTExpr::Int(_) => {
                // very correct, certainly a number is correct right?
                Type::Whole
            }
            ASTExpr::_String(_) => {
                // probs correct
                Type::String
            }
            ASTExpr::VarName(_var_idx) => Type::Whole,
            ASTExpr::FunctionCall(_func_idx, optional_expr) => {
                if let Some(func_expr) = optional_expr {
                    type_of_expr(func_expr);
                }
                Type::Unit
            }
            ASTExpr::Binary(op, expr0, expr1) => {
                let type0 = type_of_expr(expr0);
                let type1 = type_of_expr(expr1);
                match op {
                    BinaryOp::Argument => Type::Unit,
                    _ => {
                        // both have to be numbers
                        assert_eq!(Type::Whole, type0);
                        assert_eq!(Type::Whole, type1);
                        Type::Whole
                    }
                }
            }
        }
    }

    for statement in body {
        match statement {
            ASTStatement::If { condition, body } => {
                type_of_expr(condition);
                check_that_ast_is_correct(body);
            }
            ASTStatement::While { condition, body } => {
                type_of_expr(condition);
                check_that_ast_is_correct(body);
            }
            ASTStatement::EvalExpr(expr) => {
                type_of_expr(expr);
            }
        }
    }
}

struct Parser<'ParserLifetime> {
    tokens: std::iter::Peekable<std::iter::Enumerate<std::slice::Iter<'static, Token>>>,
    token_idx_to_char_nr: &'ParserLifetime Vec<(usize, usize)>,

    source: &'ParserLifetime str,
}

pub fn parse(
    tokens: &Vec<Token>,
    token_idx_to_char_nr: &Vec<(usize, usize)>,
    source: &str,
) -> ASTBody {
    let mut parser = Parser::new(tokens, token_idx_to_char_nr, source);
    parser.parse_scope()
}
enum InnerReturn {
    ASTThing(ASTExpr),
    GoBack(BinaryOp, ASTExpr),
}

impl<'ParserLifetime> Parser<'ParserLifetime> {
    fn new(
        tokens: &Vec<Token>,
        token_idx_to_char_nr: &Vec<(usize, usize)>,
        source: &'ParserLifetime str,
    ) -> Self {
        Parser {
            tokens: tokens.iter().enumerate().peekable(),
            token_idx_to_char_nr: token_idx_to_char_nr,
            source,
        }
    }
    fn eat(&mut self) -> Option<(usize, &Token)> {
        self.tokens.next()
    }

    fn peek(&mut self) -> Option<(usize, &Token)> {
        self.tokens.peek().copied()
    }

    fn report_incorrect_semantics(
        &mut self,
        msg: &str,
        bad_tok: &Token,
        token_start_pos: usize,
    ) -> ! {
        println!("Parse error: `{}`", msg);

        let (start_wher, end_wher) = self.token_idx_to_char_nr[token_start_pos];

        let mut line = 1;
        let mut col = 1;
        for (idx, ch) in self.source.chars().enumerate() {
            if idx == start_wher {
                break;
            }
            if ch == '\n' {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
        }
        panic!("{} at line {}, col {}", msg, line, col);
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
            match match self.eat() {
                Some(tok) => {
                    println!("token {:?}", tok);
                    tok
                }
                None => break,
            } {
                (_, Token::Keyword(Keyword::FunctionIncoming)) => {
                    // Next token should be function name identifier
                    let func_name = match self.eat().unwrap() {
                        (_, &Token::Identifier(func_name)) => func_name,
                        (tok_nr, tok) => self.report_incorrect_semantics(
                            &format!("Expected function name identifier"),
                            tok,
                            tok_nr,
                        ),
                    };
                    // Next token should be start paren
                    match self.eat().unwrap() {
                        (_, Token::Keyword(Keyword::StartParen)) => (),
                        (tok_nr, tok) => self.report_incorrect_semantics(
                            &format!("Expected start parenthesis after function name"),
                            tok,
                            tok_nr,
                        ),
                    };
                    // Next should be a series of name and type-names, ending with a end paren
                    let mut arg_vec = Vec::new();
                    loop {
                        let arg_name = match self.eat().unwrap() {
                            (_, &Token::Identifier(arg_name)) => arg_name,
                            (_, Token::Keyword(Keyword::EndParen)) => break,
                            (tok_nr, tok) => self.report_incorrect_semantics(
                                &format!("Expected argument name"),
                                tok,
                                tok_nr,
                            ),
                        };
                        // Next should be a colon
                        match self.eat().unwrap() {
                            (_, Token::Keyword(Keyword::TypeIncoming)) => (),
                            (n, t) => self.report_incorrect_semantics(
                                "Expected colon after argument name",
                                t,
                                n,
                            ),
                        };
                        // Type name
                        let type_name = match self.eat().unwrap() {
                            (_, &Token::Identifier(t)) => t,
                            (n, t) => self.report_incorrect_semantics("Expected type name", t, n),
                        };
                        arg_vec.push((arg_name, type_name));
                        // Next should be a comma or end paren
                        match self.eat().unwrap() {
                            (_, Token::Keyword(Keyword::Comma)) => (),
                            (_, Token::Keyword(Keyword::EndParen)) => break,
                            (n, t) => self.report_incorrect_semantics(
                                "Expected comma or end parenthesis after argument",
                                t,
                                n,
                            ),
                        };
                    }
                    // must be { after this
                    match self.eat().unwrap() {
                        (_, Token::Keyword(Keyword::StartBlock)) => (),
                        (n, t) => self.report_incorrect_semantics(
                            "Expected `{` after function declaration",
                            t,
                            n,
                        ),
                    };
                    push_to_statements(ASTStatement::Function {
                        name: func_name,
                        args: arg_vec,
                        body: self.parse_scope(),
                    });
                }

                (_, &Token::Keyword(Keyword::CreateVar)) => {
                    // Next token should be variable-name
                    let var_idx = match self.eat().unwrap() {
                        (_, &Token::Identifier(var_idx)) => var_idx,
                        (n, t) => self.report_incorrect_semantics("expected variable name", t, n),
                    };
                    // make sure there is a SET token after var name
                    match self.eat().unwrap() {
                        (_, Token::Keyword(Keyword::Set(SetType::Set))) => (),
                        (n, t) => self.report_incorrect_semantics("expected set token", t, n),
                    }
                    // now there should be an expression
                    let expr = ASTBox::new(self.parse_expr());
                    push_to_statements(ASTStatement::EvalExpr(ASTExpr::Binary(
                        BinaryOp::Set,
                        ASTBox::new(ASTExpr::VarName(var_idx)),
                        expr,
                    )));
                }
                // If statement
                (_, Token::Keyword(Keyword::If)) => {
                    let cond = self.parse_expr();
                    match self.eat().unwrap() {
                        (_, Token::Keyword(Keyword::StartBlock)) => (),
                        (n, t) => {
                            self.report_incorrect_semantics("Expected `{` after if condition", t, n)
                        }
                    };
                    let body = self.parse_scope();
                    push_to_statements(ASTStatement::If {
                        condition: ASTBox::new(cond),
                        body,
                    });
                }
                // While statement
                (_, Token::Keyword(Keyword::While)) => {
                    let cond = self.parse_expr();
                    match self.eat().unwrap() {
                        (_, Token::Keyword(Keyword::StartBlock)) => (),
                        (n, t) => self.report_incorrect_semantics(
                            "Expected `{` after while condition",
                            t,
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
                    return statements;
                }

                (n, t) => self.report_incorrect_semantics(
                    "Erroneous token to start expression with",
                    t,
                    n,
                ),
            };
        }
        return statements;
    }
    fn parse_expr(&mut self) -> ASTExpr {
        match self.parse_inner(0) {
            InnerReturn::ASTThing(expr) => return expr,
            InnerReturn::GoBack(_, _) => panic!("Can't go back in highest level???"),
        }
    }

    fn parse_leaf(&mut self) -> ASTExpr {
        match self.eat().unwrap() {
            (_, &Token::Int(v)) => ASTExpr::Int(v),
            (_, Token::Keyword(Keyword::StartParen)) => {
                // create new parse scope here
                match self.parse_inner(0) {
                    InnerReturn::ASTThing(e) => e,
                    _ => panic!("can't go back in parenthesis highest level???"),
                }
            }
            (_, &Token::Identifier(e)) => {
                // can either be a variable or a function call
                match self.peek().unwrap() {
                    (_, Token::Keyword(Keyword::StartParen)) => {
                        // function call
                        self.eat(); // eat start paren
                        let mut arg_vec = Vec::new();
                        while match self.peek().unwrap() {
                            (_, Token::Keyword(Keyword::EndParen)) => false,
                            (_, Token::Keyword(Keyword::Comma)) => true,
                            (n, t) => self.report_incorrect_semantics("expected comma", t, n),
                        } {
                            self.eat();
                            // eat all arguments
                            let arg = self.parse_expr();
                            arg_vec.push(arg);
                        }
                        self.eat(); // eat end paren
                        ASTExpr::FunctionCall(e, arg_vec)
                    }
                    _ => ASTExpr::VarName(e),
                }
            }
            (n, t @ Token::Keyword(_)) => {
                self.report_incorrect_semantics("no keywords as leaf in expr", t, n)
            }
        }
    }

    fn parse_inner(&mut self, prev_precedence: BinOpPrecedence) -> InnerReturn {
        let value = self.parse_leaf();

        let binop = match self.parse_oper(self.eat().unwrap().1) {
            Some(binop) => binop,
            None => {
                // no operator, return value
                return InnerReturn::ASTThing(value);
            }
        };
        let precedence = binop.precedence();
        if precedence < prev_precedence {
            // we have to back and collect things in this "precedence block"
            return InnerReturn::GoBack(binop, value);
        }

        let mut our_first_part = ASTBox::new(value);
        let mut our_binop = binop;

        loop {
            match self.parse_inner(precedence) {
                InnerReturn::ASTThing(e) => {
                    return InnerReturn::ASTThing(ASTExpr::Binary(
                        our_binop,
                        our_first_part,
                        ASTBox::new(e),
                    ));
                }
                InnerReturn::GoBack(lower_binop, expr) => {
                    if lower_binop.precedence() >= prev_precedence {
                        // combine, but we can continue. Contain our gotten expression into the new
                        // operator will will contain us (fine because previous precedence was
                        // similar)
                        our_first_part = ASTBox::new(ASTExpr::Binary(
                            our_binop,
                            our_first_part,
                            ASTBox::new(expr),
                        ));
                        our_binop = lower_binop;
                    } else {
                        // too low precedence, must go back
                        return InnerReturn::GoBack(
                            lower_binop,
                            ASTExpr::Binary(our_binop, our_first_part, ASTBox::new(expr)),
                        );
                    }
                }
            }
        }
    }
    fn parse_oper(&mut self, token: &Token) -> Option<BinaryOp> {
        match token {
            Token::Keyword(Keyword::Plus) => {
                return Some(BinaryOp::Add);
            }
            Token::Keyword(Keyword::Minus) => {
                return Some(BinaryOp::Sub);
            }
            Token::Keyword(Keyword::Multiply) => {
                return Some(BinaryOp::Multiply);
            }
            Token::Keyword(Keyword::Less) => {
                return Some(BinaryOp::Less);
            }
            Token::Keyword(Keyword::More) => {
                return Some(BinaryOp::More);
            }
            Token::Keyword(Keyword::Equals) => {
                return Some(BinaryOp::Equals);
            }
            Token::Keyword(Keyword::Comma) => {
                return Some(BinaryOp::Argument);
            }
            Token::Keyword(Keyword::Set(SetType::Set)) => {
                return Some(BinaryOp::Set);
            }
            Token::Keyword(Keyword::Set(SetType::Add)) => {
                return Some(BinaryOp::SetAdd);
            }
            Token::Keyword(Keyword::Set(SetType::Sub)) => {
                return Some(BinaryOp::SetSub);
            }
            Token::Keyword(Keyword::EndParen) => {
                // nothing more
                return None;
            }
            bad => panic!("Bad operator token: {:?}, ", bad),
        }
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
        match expr {
            ASTExpr::Int(num) => {
                println!("{:?}", num);
            }
            ASTExpr::_String(st) => {
                println!("{:?}", st);
            }
            ASTExpr::VarName(var_idx) => {
                println!("var: {}", var_idx);
            }
            ASTExpr::FunctionCall(name, args) => {
                println!("func: {}", name);
                if let Some(args) = args {
                    print_expr(args, indent + 1);
                }
            }
            ASTExpr::Binary(op, f, s) => {
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
            }
        }
    }

    print_body(body, 0);
}
