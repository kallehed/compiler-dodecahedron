use crate::FunctionNameIdx;
use crate::Keyword;
use crate::SetType;
use crate::Token;
use crate::VariableNameIdx;

type ASTBox<T> = Box<T>;
type ASTVec<T> = Vec<T>;
type ASTBody = ASTVec<ASTStatement>;

#[derive(Debug, Copy, Clone)]
pub enum BinaryOp {
    Argument,
    Add,
    Sub,
    Multiply,
    Equals,
}

#[derive(Debug)]
pub enum ASTExpr {
    Number(i64),
    String(&'static str),
    VarName(VariableNameIdx),
    FunctionCall(FunctionNameIdx, Option<ASTBox<ASTExpr>>),
    Binary(BinaryOp, ASTBox<ASTExpr>, ASTBox<ASTExpr>),
}

#[derive(Debug)]
pub enum ASTStatement {
    SetVar {
        var: VariableNameIdx,
        set_type: SetType,
        set_to: ASTBox<ASTExpr>,
    },
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

type TokenIter<'a> = std::slice::Iter<'a, Token>;

pub fn check_that_ast_is_correct(body: &ASTBody) {
    #[derive(Debug, PartialEq, Eq)]
    enum Type {
        Number,
        String,
        Unit,
    }
    fn type_of_expr(expr: &ASTExpr) -> Type {
        match expr {
            ASTExpr::Number(_) => {
                // very correct, certainly a number is correct right?
                Type::Number
            }
            ASTExpr::String(_) => {
                // probs correct
                Type::String
            }
            ASTExpr::VarName(_var_idx) => Type::Number,
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
                    BinaryOp::Argument => {
                        Type::Unit
                    }
                    _ => {
                        // both have to be numbers
                        assert_eq!(Type::Number, type0);
                        assert_eq!(Type::Number, type1);
                        Type::Number
                    }
                }
            }
        }
    }

    for statement in body {
        match statement {
            ASTStatement::SetVar { set_to, .. } => {
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

    loop {
        let token = match tokens.next() {
            Some(t) => t,
            None => return statements,
        };
        match token {
            &Token::Keyword(Keyword::CreateVar) => {}
            // for setting an already created variable
            &Token::VariableName(var) => {
                // next token has to be a setter
                match tokens.next().unwrap() {
                    &Token::Keyword(Keyword::Set(set_type)) => {
                        // correct.
                        let ast_statement = ASTStatement::SetVar {
                            set_type,
                            var,
                            set_to: Box::new(
                                parse_tokens_expression(tokens)
                                    .expect("No expression after trying to set variable!"),
                            ),
                        };
                        push_to_statements(ast_statement);
                    }
                    _ => panic!("Bad token after variable name"),
                }
            }
            // If statement
            Token::Keyword(Keyword::If) => {
                let cond = parse_tokens_expression(tokens).expect("No expression after if!");

                let body = parse_block(tokens);

                let ast_statement = ASTStatement::If {
                    condition: ASTBox::new(cond),
                    body,
                };
                push_to_statements(ast_statement);
            }
            // While statement
            Token::Keyword(Keyword::While) => {
                let cond = parse_tokens_expression(tokens).expect("No expression after if!");
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
                parse_tokens_expression(tokens).map(|x| ASTBox::new(x)),
            )
        }
        bad => panic!("wrong token after function name: {:?}", bad),
    }
}
fn parse_value(tokens: &mut TokenIter) -> Option<ASTExpr> {
    match tokens.next().unwrap() {
        &Token::Number(num) => {
            // we got a number, now we must find an operator
            Some(ASTExpr::Number(num))
        }
        &Token::VariableName(var_name) => Some(ASTExpr::VarName(var_name)),
        &Token::FunctionName(func_idx) => Some(parse_function_call(func_idx, tokens)),
        Token::String(st) => Some(ASTExpr::String(st)),
        Token::Keyword(Keyword::StartParen) => {
            Some(parse_tokens_expression(tokens).expect("must be tokens inside parenthesis"))
        }
        Token::NewLine => {
            // nothing here,
            return None;
        }
        bad => panic!("bad expression: {:?}", bad),
    }
}
fn parse_oper(tokens: &mut TokenIter) -> Option<BinaryOp> {
    match tokens.next().unwrap() {
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

/// parse expression formed of tokens
fn parse_tokens_expression(tokens: &mut TokenIter) -> Option<ASTExpr> {
    let first_part = match parse_value(tokens) {
        Some(v) => v,
        None => return None,
    };

    match parse_oper(tokens) {
        Some(op) => match op {
            // BinaryOp::Argument => Some(first_part),
            op @ BinaryOp::Multiply => {
                let next_value = parse_value(tokens).expect("no value after operator * !");
                let next_oper = parse_oper(tokens);
                match next_oper {
                    Some(op2) => Some(ASTExpr::Binary(
                        op2,
                        ASTBox::new(ASTExpr::Binary(
                            op,
                            ASTBox::new(first_part),
                            ASTBox::new(next_value),
                        )),
                        ASTBox::new(
                            parse_tokens_expression(tokens)
                                .expect("Must be more after an operator!"),
                        ),
                    )),
                    None => Some(ASTExpr::Binary(
                        op,
                        ASTBox::new(first_part),
                        ASTBox::new(next_value),
                    )),
                }
            }
            _ => {
                let a = Some(ASTExpr::Binary(
                    op,
                    ASTBox::new(first_part),
                    ASTBox::new(
                        parse_tokens_expression(tokens).expect("Must be more after an operator!"),
                    ),
                ));
                a
            }
        },
        None => Some(first_part),
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
            ASTExpr::Number(num) => {
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
                ASTStatement::SetVar {
                    var,
                    set_type,
                    set_to,
                } => {
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
