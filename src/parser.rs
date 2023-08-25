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

#[derive(Debug)]
pub enum ASTExpr {
    Number(i64),
    String(&'static str),
    VarName(VariableNameIdx),
    FunctionCall(FunctionNameIdx, Option<ASTBox<ASTExpr>>),
    Argument(ASTBox<ASTExpr>, ASTBox<ASTExpr>),
    Equals(ASTBox<ASTExpr>, ASTBox<ASTExpr>),
    Add(ASTBox<ASTExpr>, ASTBox<ASTExpr>),
    Multiply(ASTBox<ASTExpr>, ASTBox<ASTExpr>),
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

fn parse_tokens_expression(tokens: &mut TokenIter) -> Option<ASTExpr> {
    let token = tokens.next().unwrap();
    let first_part = match token {
        &Token::Number(num) => {
            // we got a number, now we must find an operator
            ASTExpr::Number(num)
        }
        &Token::VariableName(var_name) => ASTExpr::VarName(var_name),
        &Token::FunctionName(function_name) => {
            match tokens.next().unwrap() {
                Token::Keyword(Keyword::StartParen) => {
                    // we have a function call, get the expression inside, and this will be the
                    // first part
                    ASTExpr::FunctionCall(
                        function_name,
                        parse_tokens_expression(tokens).map(|x| ASTBox::new(x)),
                    )
                }
                bad => panic!("wrong token after function name: {:?}", bad),
            }
        }
        Token::String(st) => {
            ASTExpr::String(st)
        }
        Token::NewLine => {
            // nothing here,
            return None;
        }
        bad => panic!("bad expression: {:?}", bad),
    };
    match tokens.next().unwrap() {
        Token::Keyword(Keyword::Plus) => {
            let ast_expr = ASTExpr::Add(
                ASTBox::new(first_part),
                ASTBox::new(
                    parse_tokens_expression(tokens).expect("Must be more after a plus sign!"),
                ),
            );
            return Some(ast_expr);
        }
        Token::Keyword(Keyword::Minus) => {
            let ast_expr = ASTExpr::Add(
                ASTBox::new(first_part),
                ASTBox::new(ASTExpr::Neg(ASTBox::new(
                    parse_tokens_expression(tokens).expect("Must be more after minus sign!"),
                ))),
            );
            return Some(ast_expr);
        }
        Token::Keyword(Keyword::Multiply) => {
            let ast_expr = ASTExpr::Multiply(
                ASTBox::new(first_part),
                ASTBox::new(
                    parse_tokens_expression(tokens).expect("Must be more after multiply sign!"),
                ),
            );
            return Some(ast_expr);
        }
        Token::NewLine | Token::Keyword(Keyword::EndParen) => {
            // nothing more
            return Some(first_part);
        }
        Token::Keyword(Keyword::Comma) => {
            let ast_expr = ASTExpr::Argument(
                ASTBox::new(first_part),
                ASTBox::new(
                    parse_tokens_expression(tokens)
                        .expect("Nothing after comma(,) in function call!"),
                ),
            );
            return Some(ast_expr);
        }
        bad => panic!("Bad token after first part: {:?}, first_part: {:?}", bad, first_part),
    }
}
