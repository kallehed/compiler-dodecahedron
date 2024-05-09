use crate::parser::{ASTBody, ASTExpr, InASTExpr, ASTStatement};



pub fn type_check_ast(body: &ASTBody) {
    
    struct State {

    }
    impl State {
        fn type_check_body(body: &ASTBody) {
            for stat in body {
                match stat {
                    ASTStatement::If { condition, body } => {

                    }
                    ASTStatement::While { condition, body } => todo!(),
                    ASTStatement::EvalExpr(_) => todo!(),
                    ASTStatement::CreateVar(_) => todo!(),
                    ASTStatement::Function { name, args, body } => todo!(),
                }
            }
        }
        fn type_check_expr(expr: &ASTExpr) {
            match expr.0 {
                InASTExpr::Int(_) => todo!(),
                InASTExpr::String(_) => todo!(),
                InASTExpr::VarName(_) => todo!(),
                InASTExpr::FunctionCall(_, _) => todo!(),
                InASTExpr::Binary(_, _, _) => todo!(),
            }
        }
    }

    
}