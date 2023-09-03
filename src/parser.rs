use crate::FunctionNameIdx;
use crate::Keyword;
use crate::SetType;
use crate::Token;
use crate::Type;
use crate::VariableNameIdx;

type ASTBox<T> = Box<T>;
type ASTVec<T> = Vec<T>;
type ASTBody = ASTVec<ASTStatement>;

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum BinaryOp {
    // numbers specify their precedence. Higher means it should be evaluated first
    Argument = 0,
    Equals = 1,
    Add = 2,
    Sub = 3,
    Multiply = 4,
}

impl BinaryOp {
    fn least() -> Self {
        Self::Argument
    }
}

#[derive(Debug)]
pub enum ASTExpr {
    Whole(i64),
    String(&'static str),
    VarName(VariableNameIdx),
    FunctionCall(FunctionNameIdx, Option<ASTBox<ASTExpr>>),
    Binary(BinaryOp, ASTBox<ASTExpr>, ASTBox<ASTExpr>),
}

#[derive(Debug)]
pub struct ASTSetVar {
    var_idx: VariableNameIdx,
    set_type: SetType, // === or +== or -==
    set_to: ASTBox<ASTExpr>,
}

#[derive(Debug)]
pub enum ASTStatement {
    CreateVar(Type, ASTSetVar),
    SetVar(ASTSetVar),
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
            ASTExpr::String(_) => {
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
            ASTStatement::CreateVar(type_id, set_var) => {
                let expr_type = type_of_expr(&set_var.set_to);
                assert_eq!(*type_id, expr_type);
            }
            ASTStatement::SetVar(ASTSetVar { set_to, .. }) => {
                type_of_expr(set_to);
            }
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

    fn parse_variable(tokens: &mut TokenIter, var_idx: VariableNameIdx) -> ASTSetVar {
        // next token has to be a setter
        match tokens.next().unwrap() {
            &Token::Keyword(Keyword::Set(set_type)) => {
                // correct.
                ASTSetVar {
                    set_type,
                    var_idx,
                    set_to: Box::new(
                        parse_expression_start(tokens)
                            .expect("No expression after trying to set variable!"),
                    ),
                }
            }
            _ => panic!("Bad token after variable name"),
        }
    }

    loop {
        let token = match tokens.next() {
            Some(t) => t,
            None => return statements,
        };
        match token {
            &Token::Keyword(Keyword::CreateVar) => {
                // Next token should be type
                match tokens.next().unwrap() {
                    Token::Keyword(Keyword::Type(Type::Whole)) => match tokens.next().unwrap() {
                        // get variable name
                        &Token::VariableName(var_idx) => {
                            let ast_set = parse_variable(tokens, var_idx);
                            let statement = ASTStatement::CreateVar(Type::Whole, ast_set);
                            push_to_statements(statement);
                        }
                        bad => panic!("bad token after creating var and giving type: {:?}", bad),
                    },
                    bad => panic!("Invalid token after creating variable: {:?}", bad),
                }
            }
            // for setting an already created variable
            &Token::VariableName(var_idx) => {
                let ast_set = parse_variable(tokens, var_idx);
                let statement = ASTStatement::SetVar(ast_set);
                push_to_statements(statement);
            }
            // If statement
            Token::Keyword(Keyword::If) => {
                let cond = parse_expression_start(tokens).expect("No expression after if!");

                let body = parse_block(tokens);

                let ast_statement = ASTStatement::If {
                    condition: ASTBox::new(cond),
                    body,
                };
                push_to_statements(ast_statement);
            }
            // While statement
            Token::Keyword(Keyword::While) => {
                let cond = parse_expression_start(tokens).expect("No expression after if!");
                let body = parse_block(tokens);
                let ast_statement = ASTStatement::While {
                    condition: ASTBox::new(cond),
                    body,
                };
                push_to_statements(ast_statement);
            }
            &Token::FunctionName(func_idx) => {
                // Call function
                let ast_statement = ASTStatement::EvalExpr(parse_function_call(func_idx, tokens));
                push_to_statements(ast_statement);
            }
            Token::Keyword(Keyword::End) => {
                return statements;
            }
            Token::NewLine => (),

            bad => panic!("Invalid token for start of statement: {:?}", bad),
        }
    }
}

fn parse_function_call(func_idx: FunctionNameIdx, tokens: &mut TokenIter) -> ASTExpr {
    match tokens.next().unwrap() {
        Token::Keyword(Keyword::StartParen) => {
            // we have a function call, get the expression inside, and this will be the
            // first part
            ASTExpr::FunctionCall(
                func_idx,
                parse_expression_start(tokens).map(|x| ASTBox::new(x)),
            )
        }
        bad => panic!("wrong token after function name: {:?}", bad),
    }
}

fn parse_value(tokens: &mut TokenIter) -> Option<ASTExpr> {
    match tokens.next().unwrap() {
        &Token::Whole(num) => {
            // we got a number, now we must find an operator
            Some(ASTExpr::Whole(num))
        }
        &Token::VariableName(var_name) => Some(ASTExpr::VarName(var_name)),
        &Token::FunctionName(func_idx) => Some(parse_function_call(func_idx, tokens)),
        Token::String(st) => Some(ASTExpr::String(st)),
        Token::Keyword(Keyword::StartParen) => {
            Some(parse_expression_start(tokens).expect("must be tokens inside parenthesis"))
        }
        Token::NewLine => {
            // nothing here,
            return None;
        }
        bad => panic!("bad expression: {:?}", bad),
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
        Token::Keyword(Keyword::Equals) => {
            return Some(BinaryOp::Equals);
        }
        Token::Keyword(Keyword::Comma) => {
            return Some(BinaryOp::Argument);
        }
        Token::NewLine | Token::Keyword(Keyword::EndParen) => {
            // nothing more
            return None;
        }
        bad => panic!("Bad token after first part: {:?}, ", bad),
    }
}

fn parse_expression(tokens: &mut TokenIter, mut lhs: ASTExpr, min_precedence: BinaryOp) -> ASTExpr {
    let mut lookahead = parse_oper(tokens.peek().unwrap()).unwrap();
    while lookahead >= min_precedence {
        let op = lookahead;
        tokens.next();
        let mut rhs = parse_value(tokens).unwrap();
        lookahead = match parse_oper(tokens.peek().unwrap()) {
            Some(a) => a,
            None => break,
        };
        while lookahead > op {
            rhs = parse_expression(tokens, rhs, op);
            lookahead = parse_oper(tokens.peek().unwrap()).unwrap();
        }
        lhs = ASTExpr::Binary(op, ASTBox::new(lhs), ASTBox::new(rhs));
    }
    return lhs;
}


/// parse expression formed of tokens
fn parse_expression_start(tokens: &mut TokenIter) -> Option<ASTExpr> {
    let value = match parse_value(tokens) {
        Some(v) => v,
        None => return None,
    };
    Some(parse_expression(tokens, value, BinaryOp::least()))
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
            ASTExpr::String(st) => {
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
                ASTStatement::CreateVar(
                    type_id,
                    ASTSetVar {
                        var_idx: var,
                        set_type,
                        set_to,
                    },
                ) => {
                    println!(
                        "CREATE type: {:?}, Var {} set: {:?}",
                        type_id, var, set_type
                    );
                    print_expr(set_to, indent + 1);
                }
                ASTStatement::SetVar(ASTSetVar {
                    var_idx: var,
                    set_type,
                    set_to,
                }) => {
                    println!("Var {} set: {:?}", var, set_type);
                    print_expr(set_to, indent + 1);
                }
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
