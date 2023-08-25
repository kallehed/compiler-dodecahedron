use crate::FunctionNameIdx;
use crate::Keyword;
use crate::SetType;
use crate::Token;
use crate::VariableNameIdx;
use crate::STRING_DELIMITER;
use crate::VARIABLE_PREFIX;

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
    Neg(ASTBox<ASTExpr>),
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
        else_body: Option<ASTBody>,
    },
    While {
        condition: ASTBox<ASTExpr>,
    },
}

type TokenIter<'a> = std::slice::Iter<'a, Token>;

/// Variable name => Set it to expression
///

pub fn parse_tokens(tokens: &[Token]) {
    let mut tokens = tokens.iter();
    loop {
        let token = tokens.next().unwrap();
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
                                parse_tokens_expression(&mut tokens)
                                    .expect("No expression after trying to set variable!"),
                            ),
                        };
                        println!("got an AST: {:?}", ast_statement);
                    }
                    _ => panic!("Bad token after variable name"),
                }
            }
            Token::NewLine => (),

            bad => panic!("Invalid token for start of statement: {:?}", bad),
        }
    }
}

/// parse expression formed of tokens
fn parse_tokens_expression(tokens: &mut TokenIter) -> Option<ASTExpr> {
    fn parse_value(tokens: &mut TokenIter) -> Option<ASTExpr> {
        match tokens.next().unwrap() {
            &Token::Number(num) => {
                // we got a number, now we must find an operator
                Some(ASTExpr::Number(num))
            }
            &Token::VariableName(var_name) => Some(ASTExpr::VarName(var_name)),
            &Token::FunctionName(function_name) => {
                match tokens.next().unwrap() {
                    Token::Keyword(Keyword::StartParen) => {
                        // we have a function call, get the expression inside, and this will be the
                        // first part
                        Some(ASTExpr::FunctionCall(
                            function_name,
                            parse_tokens_expression(tokens).map(|x| ASTBox::new(x)),
                        ))
                    }
                    bad => panic!("wrong token after function name: {:?}", bad),
                }
            }
            Token::String(st) => Some(ASTExpr::String(st)),
            Token::NewLine => {
                // nothing here,
                return None;
            }
            bad => panic!("bad expression: {:?}", bad),
        }
    }

    let first_part = match parse_value(tokens) {
        Some(v) => v,
        None => return None,
    };

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
    match parse_oper(tokens) {
        Some(op) => match op {
            BinaryOp::Multiply => {
                let next_value = parse_value(tokens).expect("no value after operator * !");
                let next_oper = parse_oper(tokens);
                match next_oper {
                    Some(op2) => Some(ASTExpr::Binary(
                        op2,
                        ASTBox::new(ASTExpr::Binary(
                            BinaryOp::Multiply,
                            ASTBox::new(first_part),
                            ASTBox::new(next_value),
                        )),
                        ASTBox::new(
                            parse_tokens_expression(tokens)
                                .expect("Must be more after an operator!"),
                        ),
                    )),
                    None => Some(ASTExpr::Binary(
                        BinaryOp::Multiply,
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
