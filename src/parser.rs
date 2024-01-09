use crate::ASTBody;
use crate::FunctionNameIdx;
use crate::Keyword;
use crate::SetType;
use crate::Token;
use crate::Type;
use crate::VariableNameIdx;
use crate::Int;

type ASTBox<T> = Box<T>;

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
    Whole(Int),
    _String(&'static str),
    VarName(VariableNameIdx),
    _FunctionCall(FunctionNameIdx, Option<ASTBox<ASTExpr>>),
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
}

type TokenIter<'a> = std::iter::Peekable<std::slice::Iter<'a, Token>>;

pub fn check_that_ast_is_correct(body: &ASTBody) {
    fn type_of_expr(expr: &ASTExpr) -> Type {
        match expr {
            ASTExpr::Whole(_) => {
                // very correct, certainly a number is correct right?
                Type::Whole
            }
            ASTExpr::_String(_) => {
                // probs correct
                Type::String
            }
            ASTExpr::VarName(_var_idx) => Type::Whole,
            ASTExpr::_FunctionCall(_func_idx, optional_expr) => {
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

/// Parse generalized tokens. Probably statements.
/// Variable name => Set it to expression
/// If => generate If node
pub fn parse_block(tokens: &mut TokenIter) -> ASTBody {
    let mut statements = ASTBody::new();
    let mut push_to_statements = |statement| {
        println!("got an AST: {:?}", statement);
        statements.push(statement)
    };

    loop {
        let token = match tokens.next() {
            Some(t) => t,
            None => return statements,
        };
        let statement = match token {
            &Token::Keyword(Keyword::CreateVar) => {
                // Next token should be type
                // type not currently used TODO
                let _var_type = match tokens.next().unwrap() {
                    Token::Keyword(Keyword::Type(created_type)) => created_type,
                    bad => panic!("Invalid token after creating variable: {:?}", bad),
                };
                // get variable name
                let var_idx = match tokens.next().unwrap() {
                    &Token::VariableName(var_idx) => var_idx,
                    bad => panic!("Should have given variable name, got: {:?}", bad),
                };
                // make sure there is a SET token after var name
                match tokens.next().unwrap() {
                    Token::Keyword(Keyword::Set(SetType::Set)) => (),
                    bad => panic!("Should have given SET token, got: {:?}", bad),
                }
                // now there should be an expression
                let expr = ASTBox::new(parse_expr(tokens));
                ASTStatement::EvalExpr(ASTExpr::Binary(
                    BinaryOp::Set,
                    ASTBox::new(ASTExpr::VarName(var_idx)),
                    expr,
                ))
            }
            // If statement
            Token::Keyword(Keyword::If) => {
                let cond = parse_expr(tokens);
                let body = parse_block(tokens);
                ASTStatement::If {
                    condition: ASTBox::new(cond),
                    body,
                }
            }
            // While statement
            Token::Keyword(Keyword::While) => {
                let cond = parse_expr(tokens);
                let body = parse_block(tokens);
                ASTStatement::While {
                    condition: ASTBox::new(cond),
                    body,
                }
            }
            Token::Keyword(Keyword::Invoke) => {
                let expr = parse_expr(tokens);
                ASTStatement::EvalExpr(expr)
            }
            Token::Keyword(Keyword::End) => {
                return statements;
            }
            Token::NewLine => continue,

            bad => panic!("Invalid token for start of statement: {:?}", bad),
        };
        push_to_statements(statement);
    }
}
fn parse_expr(tokens: &mut TokenIter) -> ASTExpr {
    enum InnerReturn {
        ASTThing(ASTExpr),
        GoBack(BinaryOp, ASTExpr),
    }
    fn inner(tokens: &mut TokenIter, prev_precedence: BinOpPrecedence) -> InnerReturn {
        let value = match tokens.next().unwrap() {
            Token::String(_) => panic!("no strings allowed in expressions"),
            &Token::Int(v) => ASTExpr::Whole(v),
            Token::Keyword(Keyword::StartParen) => {
                // create new parse group here
                match inner(tokens, 0) {
                    InnerReturn::ASTThing(e) => e,
                    _ => panic!("can't go back in parenthesis highest level???"),
                }
            }
            &Token::VariableName(e) => ASTExpr::VarName(e),
            Token::FunctionName(name) => {
                let Token::Keyword(Keyword::StartParen) = tokens.next().unwrap() else {
                    panic!("No startparen after function name!");
                };
                match inner(tokens, 0) {
                    InnerReturn::ASTThing(e) => ASTExpr::_FunctionCall(*name, Some(ASTBox::new(e))),
                    _ => panic!("function paren not work!"),
                }

            }
            Token::NewLine => panic!("no value found?????"),
            Token::Keyword(bad) => panic!("no keywords in expressions: {:?}", bad),
        };

        let binop = match parse_oper(tokens.next().unwrap()) {
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
            match inner(tokens, precedence) {
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
    match inner(tokens, 0) {
        InnerReturn::ASTThing(expr) => return expr,
        InnerReturn::GoBack(_, _) => panic!("Can't go back in highest level???"),
    }
}

fn parse_oper(token: &Token) -> Option<BinaryOp> {
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
        Token::NewLine | Token::Keyword(Keyword::EndParen) => {
            // nothing more
            return None;
        }
        bad => panic!("Bad operator token: {:?}, ", bad),
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
            ASTExpr::Whole(num) => {
                println!("{:?}", num);
            }
            ASTExpr::_String(st) => {
                println!("{:?}", st);
            }
            ASTExpr::VarName(var_idx) => {
                println!("var: {}", var_idx);
            }
            ASTExpr::_FunctionCall(name, args) => {
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
