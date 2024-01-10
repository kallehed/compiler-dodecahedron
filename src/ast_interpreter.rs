use crate::parser::{ASTBody, ASTExpr, ASTStatement, BinaryOp};
use crate::Int;

#[derive(Default)]
struct Interpreter {
    wholes: std::collections::HashMap<VariableNameIdx, Int>,
}

impl Interpreter {
    fn set_whole(&mut self, name: VariableNameIdx, val: Int) {
        self.wholes.insert(name, val);
    }
    fn get_whole(&mut self, name: VariableNameIdx) -> Int {
        *self.wholes.get(&name).unwrap()
    }
}

pub fn run_ast(ast: &ASTBody) {
    let mut interp = Interpreter::default();

    run_ast_with_interp(&mut interp, ast);
}

fn run_ast_with_interp(interp: &mut Interpreter, ast: &ASTBody) {
    fn run_expr(interp: &mut Interpreter, expr: &ASTExpr) -> Int {
        match expr {
            &ASTExpr::Int(whole) => return whole,
            ASTExpr::_String(_s) => todo!(),
            ASTExpr::VarName(_) => todo!(),
            ASTExpr::FunctionCall(name, args) => {

                let o = args.as_ref().unwrap();
                let val = run_expr(interp, &o);
                println!("function called on: {:?}", val);
            }
            ASTExpr::Binary(binop, left, right) => match binop {
                operation @ (BinaryOp::SetAdd | BinaryOp::SetSub | BinaryOp::Set) => {
                    let ASTExpr::VarName(name) = **left else {
                        panic!("l-value not a variable name!");
                    };
                    let val = run_expr(interp, right);
                    let res = match operation {
                        BinaryOp::SetAdd => interp.get_whole(name) + val,
                        BinaryOp::SetSub => interp.get_whole(name) - val,
                        BinaryOp::Set => val,
                        _ => todo!(),
                    };
                    interp.set_whole(name, res);
                }
                BinaryOp::Argument => {}
                BinaryOp::Equals => {}
                BinaryOp::Less => {}
                BinaryOp::More => {}
                BinaryOp::Add => {}
                BinaryOp::Sub => {}
                BinaryOp::Multiply => {}
            },
        };
        69420
    }

    fn run_statement(interp: &mut Interpreter, stat: &ASTStatement) {
        match stat {
            ASTStatement::If { condition, body } => {
                if run_expr(interp, condition) != 0 {
                    run_ast_with_interp(interp, body);
                }
            }
            ASTStatement::While { condition, body } => {
                while run_expr(interp, condition) != 0 {
                    run_ast_with_interp(interp, body);
                }
            }
            ASTStatement::EvalExpr(expr) => {
                run_expr(interp, expr);
            }
        }
    }

    for stat in ast.iter() {
        run_statement(interp, stat);
    }
}
