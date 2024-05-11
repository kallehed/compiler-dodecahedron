use std::collections::HashMap;

use crate::{
    parser::{ASTBody, ASTExpr, ASTStatement, BinaryOp, InASTExpr},
    IdentIdx,
};

pub fn type_check_ast<'a>(
    body: &'a ASTBody,
    ident_to_name: &[&'static str],
    functions: &HashMap<IdentIdx, u16>,
    source: &'a str,
    file_name: &'a str,
    token_idx_to_char_range: &[(usize, usize)],
) {
    struct State<'b> {
        /// function ident and nr of parameters
        functions: &'b HashMap<IdentIdx, u16>,
        function_calls: Vec<&'b ASTExpr>,

        // constant
        ident_to_name: &'b [&'static str],
        source: &'b str,
        file_name: &'b str,
        token_idx_to_char_range: &'b [(usize, usize)],
    }
    #[derive(Debug, Clone, Copy, Eq, PartialEq)]
    enum Type {
        /// Returned by expressions such as x = 3;
        Unit,
        /// standard type for language currently
        Int,
    }
    impl State<'_> {
        const TOP_LEVEL_SCOPE: u64 = 1;
        /// scope starts at 0 but will be immediately inc:ed to 1.
        fn type_check_scope(&mut self, body: &ASTBody, scope: u64) {
            let scope = scope + 1;
            for stat in body {
                match stat {
                    ASTStatement::If { condition, body } => {
                        match self.type_check_expr(condition) {
                            // condition has to be Int
                            Type::Int => {}
                            other => {
                                panic!("Condition to `if` can't have type: {:?}", other);
                            }
                        }
                        self.type_check_scope(body, scope);
                    }
                    ASTStatement::While { condition, body } => {
                        match self.type_check_expr(condition) {
                            // condition has to be Int
                            Type::Int => {}
                            other => {
                                panic!("Condition to `while loop` can't have type: {:?}", other);
                            }
                        }
                        self.type_check_scope(body, scope);
                    }
                    ASTStatement::EvalExpr(expr) => {
                        self.type_check_expr(expr);
                    }
                    ASTStatement::CreateVar(_) => (), // TODO, check that variables are created before use.
                    ASTStatement::Function { name, args, body } => {
                        // function already checked to be correct in parsing stage
                        // TODO make sure that function args are added to local variables in next scope when checking for variable definitions.
                        self.type_check_scope(body, scope);
                    }
                }
            }
        }

        fn report_error_on_token_idx(&mut self, msg: &str,  token_idx: usize) -> ! {
            crate::mark_error_in_source(self.source, self.file_name, msg, self.token_idx_to_char_range[token_idx]);
            std::process::exit(1);
        }
        /// TODO inline/remove if only is used once
        fn type_correct(&mut self, should_be: Type, is: Type, token_idx: usize) {
            if should_be == is {
                return;
            }
            self.report_error_on_token_idx(&format!("Expected type `{:?}`, received type: `{:?}`!", should_be, is), token_idx);
        }

        fn type_check_expr(&mut self, expr: &ASTExpr) -> Type {
            match &expr.0 {
                InASTExpr::Int(_) => Type::Int,     // int is probably correct
                InASTExpr::VarName(_) => Type::Int, // vars are int, so probably correct
                InASTExpr::FunctionCall(name, args) =>
                // make sure that function is called with correct number of arguments.
                {
                    // function exists, and called with right number of arguments
                    match self.functions.get(name) {
                        Some(&nr_of_args) => {
                            if args.len() != nr_of_args as _ {
                                self.report_error_on_token_idx(&format!("Function `{}` called with {} argument(s), but it needs {}", &self.ident_to_name[*name as usize], args.len(),  nr_of_args), expr.1)
                            }
                        }
                        None => {
                            // function literally doesn't exist, why are you trying to call a nonexistent function?
                            self.report_error_on_token_idx(&format!("Tried to call function `{}` which doesn't exist", &self.ident_to_name[*name as usize]), expr.1);
                        }
                    }
                    Type::Int
                }
                InASTExpr::Binary(op, left, right) => {
                    match op {
                        BinaryOp::Set | BinaryOp::SetAdd | BinaryOp::SetSub => {
                            // when setting, left side has to be lvalue
                            if let Some(msg) = match left.0 {
                                InASTExpr::Int(_) => Some("Int literal"),
                                InASTExpr::VarName(_) => None,
                                InASTExpr::FunctionCall(_, _) => {
                                    Some("function call")
                                }
                                InASTExpr::Binary(op2, _, _) => {
                                    let string: &'static str = format!("binary operator {:?}", op2).leak();
                                    Some(string)
                                }
                            } {
                                self.report_error_on_token_idx(&format!("lvalue to setter `{:?}` can't be: `{}`", op, msg), left.1);
                            }

                            // right side has to be Int
                            let right_type = self.type_check_expr(right);
                            self.type_correct(Type::Int, right_type, right.1);
                            Type::Unit
                        }
                        BinaryOp::Equals | BinaryOp::Less | BinaryOp::More | BinaryOp::Add | BinaryOp::Sub | BinaryOp::Multiply => {
                            let left_type = self.type_check_expr(left);
                            let right_type = self.type_check_expr(right);
                            if left_type != right_type { // TODO: print types without debug print
                                self.report_error_on_token_idx(&format!("Binary operator `{:?}` received incompatible types: `{:?}`, and: `{:?}`!", op, left_type, right_type), expr.1);
                            }

                            Type::Int
                        }
                    }
                }
            }
        }
    }
    let mut s = State {
        functions,
        function_calls: Vec::new(),

        ident_to_name,
        source,
        file_name,
        token_idx_to_char_range
    };
    s.type_check_scope(body, 0);
}
