use crate::{
    parser::{ASTBody, ASTExpr, ASTStatement, InASTStatement, BinaryOp, InASTExpr},
    IdentIdx,
};

const DEFAULT_TYPE: &str = "int64_t ";

pub fn to_c_code(body: &ASTBody, ident_to_string: &[&'static str]) -> String {
    struct Output<'a> {
        code: String,
        declarations: String,

        // immutable
        ident_to_string: &'a [&'static str],
    }

    impl<'a> Output<'a> {
        fn print(&mut self, to_out: &str) {
            self.code.push_str(to_out);
        }
        fn declare(&mut self, to_out: &str) {
            self.declarations.push_str(to_out);
        }

        fn function_to_c_name(&self, func: &IdentIdx) -> String {
            let real_name = self.ident_to_string[*func as usize];
            match real_name {
                "print_int" => "print_int".to_string(),
                "main" => "main".to_string(),
                _ => format!("func{}", func),
            }
        }

        fn var_to_c_name(&self, var: &IdentIdx) -> String {
            format!("var{}", var)
        }

        fn binary_op_to_c_name(&self, op: &BinaryOp) -> &'static str {
            match op {
                BinaryOp::SetAdd => "+=",
                BinaryOp::SetSub => "-=",
                BinaryOp::Set => "=",
                BinaryOp::Equals => "==",
                BinaryOp::Less => "<",
                BinaryOp::More => ">",
                BinaryOp::Add => "+",
                BinaryOp::Sub => "-",
                BinaryOp::Multiply => "*",
            }
        }

        fn function_to_c_start_of_function(&self, name: &IdentIdx, args: &[IdentIdx]) -> String {
            let mut out = String::new();

            out.push_str(DEFAULT_TYPE);
            out.push_str(&self.function_to_c_name(name));
            out.push('(');
            for (idx, arg) in args.iter().enumerate() {
                out.push_str(DEFAULT_TYPE);
                out.push_str(&self.var_to_c_name(arg));
                if idx == args.len() - 1 {
                    break;
                }
                out.push(',');
            }
            out.push(')');
            out
        }

        fn body_to_c(&mut self, body: &ASTBody) {
            for stat in body {
                match &stat.0 {
                    InASTStatement::If { condition, body } => {
                        self.print("if (");
                        self.expr_to_c(condition);
                        self.print("){");
                        self.body_to_c(body);
                        self.print("}");
                    }
                    InASTStatement::While { condition, body } => {
                        self.print("while (");
                        self.expr_to_c(condition);
                        self.print("){");
                        self.body_to_c(body);
                        self.print("}");
                    }
                    InASTStatement::EvalExpr(expr) => {
                        self.expr_to_c(expr);
                        self.print(";");
                    }
                    InASTStatement::Function { name, args, body } => {
                        let first_part = self.function_to_c_start_of_function(name, args);
                        self.print(&first_part);
                        self.declare(&first_part);
                        self.declare(";");

                        self.print("{");
                        self.body_to_c(body);
                        self.print("}");
                    }
                    InASTStatement::CreateVar(var_name) => {
                        self.print(DEFAULT_TYPE);
                        self.print(&self.var_to_c_name(var_name));
                        self.print(";");
                    }
                }
            }
        }

        fn expr_to_c(&mut self, expr: &ASTExpr) {
            match &expr.0 {
                InASTExpr::Int(int) => {
                    self.print(&int.to_string());
                }
                InASTExpr::VarName(var_name) => {
                    self.print(&self.var_to_c_name(var_name));
                }
                InASTExpr::FunctionCall(name, args) => {
                    self.print(&self.function_to_c_name(name));
                    self.print("(");
                    for (idx, arg) in args.iter().enumerate() {
                        self.expr_to_c(arg);
                        if idx == args.len() - 1 {
                            break;
                        }
                        self.print(",");
                    }
                    self.print(")");
                }
                InASTExpr::Binary(op, left, right) => {
                    // same for L-value binarys and normal exprs
                    self.print("((");

                    self.expr_to_c(left);

                    self.print(")");

                    self.print(self.binary_op_to_c_name(op));

                    self.print("(");

                    self.expr_to_c(right);

                    self.print("))");
                }
            }
        }
    }

    let mut out = Output {
        code: String::new(),
        declarations: "#include <stdio.h>\n#include <stdint.h> \n void print_int(int64_t x){printf(\"%ld\\n\", x);}\n".to_string(),
        ident_to_string,
    };

    out.body_to_c(body);
    out.declarations.push_str(&out.code);
    out.declarations
}
