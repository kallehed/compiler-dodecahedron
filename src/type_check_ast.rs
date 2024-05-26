use std::collections::{HashMap, HashSet};

use crate::{
    parser::{ASTBody, ASTExpr, BinaryOp, InASTExpr, InASTStatement},
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
        function_calls: Vec<&'b ASTExpr>,
        /// O(1) local var exists checker
        current_vars: HashSet<IdentIdx>,

        // constant
        functions: &'b HashMap<IdentIdx, u16>,
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
        /// scope starts at 0 but will be immediately inc:ed to 1.
        fn type_check_scope(&mut self, body: &ASTBody, scope: u64) {
            let scope = scope + 1;

            // holds names of local variables used in this scope.
            // Used to delete them from the 'global' set at end of this function
            let mut vars_in_scope = Vec::new();
            for stat in body {
                match &stat.0 {
                    InASTStatement::If { condition, body } => {
                        match self.type_check_expr(condition) {
                            // condition has to be Int
                            Type::Int => {}
                            other => {
                                // TODO: now we only mark the If token, but maybe we should mark
                                // the condition tokens as well? Maybe when implementing better
                                // error messages.
                                self.report_error_on_token_idx(
                                    &format!("Condition to `if` can't have type: {:?}", other),
                                    stat.1,
                                );
                            }
                        }
                        self.type_check_scope(body, scope);
                    }
                    InASTStatement::While { condition, body } => {
                        match self.type_check_expr(condition) {
                            // condition has to be Int
                            Type::Int => {}
                            other => {
                                self.report_error_on_token_idx(
                                    &format!("Condition to `while` can't have type: {:?}", other),
                                    stat.1,
                                );
                            }
                        }
                        self.type_check_scope(body, scope);
                    }
                    InASTStatement::EvalExpr(expr) => {
                        self.type_check_expr(expr);
                    }
                    // TODO, check that variables are created before use.
                    InASTStatement::CreateVar(name) => {
                        // can't re-create variable (no variable shadowing)
                        if self.current_vars.contains(name) {
                            // ERROR: variable already exists!
                            // TODO: report on correct token
                            self.report_error_on_token_idx(
                                &format!(
                                    "variable already exists, can't redeclare variable: `{}`",
                                    self.ident_to_name[*name as usize]
                                ),
                                stat.1,
                            );
                        }
                        // TODO: make sure that variable is deleted after scope ends
                        self.current_vars.insert(*name);
                        vars_in_scope.push(name);
                    }
                    InASTStatement::Function {
                        name: _,
                        args,
                        body,
                    } => {
                        // function already checked to be correct in parsing stage
                        for &arg in args {
                            // can function arguments shadow variables outside? You can't have
                            // arguments outside, so this is a non problem. Though we do special
                            // check to make sure nothing wacky happens though:
                            assert!(self.current_vars.insert(arg));
                        }

                        self.type_check_scope(body, scope);
                        // TODO: refactor this so that this is automatically handled. Or maybe not,
                        // if no-one else needs this functionality

                        // remove variables again
                        for arg in args {
                            assert!(self.current_vars.remove(arg));
                        }
                    }
                    InASTStatement::Return(expr) => {
                        self.type_check_expr(expr);
                    }
                }
            }

            // remove local variables from 'global' variable hashset
            for var_name in vars_in_scope {
                assert!(self.current_vars.remove(var_name),
                "local variables removed must exist, must not have been removed by someone else beforehand"
                );
            }
        }

        fn report_error_on_token_idx(&mut self, msg: &str, token_idx: usize) -> ! {
            crate::mark_error_in_source(
                self.source,
                self.file_name,
                msg,
                self.token_idx_to_char_range[token_idx],
            );
            std::process::exit(1);
        }

        fn possibly_report_variable_nonexistance(&mut self, ident: IdentIdx, token_idx: usize) {
            if !self.current_vars.contains(&ident) {
                self.report_error_on_token_idx(
                    &format!(
                        "Variable `{}` must be declared before usage!",
                        self.ident_to_name[ident as usize]
                    ),
                    token_idx,
                );
            }
        }

        fn type_check_expr(&mut self, expr: &ASTExpr) -> Type {
            match &expr.0 {
                InASTExpr::Int(_) => Type::Int, // int is probably correct
                // varname must exist to be used!
                InASTExpr::VarName(name) => {
                    if !self.current_vars.contains(name) {
                        // ERROR: tried to reference nonexistent variable
                        self.report_error_on_token_idx("Variable has not been declared!", expr.1);
                    }
                    Type::Int // variable exists, and all variables are Int
                }
                InASTExpr::FunctionCall(name, args) =>
                // make sure that function is called with correct number of arguments.
                {
                    // function exists, and called with right number of arguments
                    match self.functions.get(name) {
                        Some(&nr_of_args) => {
                            if args.len() != nr_of_args as _ {
                                self.report_error_on_token_idx(
                                    &format!(
                                        "Function `{}` called with {} argument(s), but it needs {}",
                                        &self.ident_to_name[*name as usize],
                                        args.len(),
                                        nr_of_args
                                    ),
                                    expr.1,
                                )
                            }
                        }
                        None => {
                            // function literally doesn't exist, why are you trying to call a nonexistent function?
                            self.report_error_on_token_idx(
                                &format!(
                                    "Tried to call function `{}` which doesn't exist",
                                    &self.ident_to_name[*name as usize]
                                ),
                                expr.1,
                            );
                        }
                    }
                    // check correctness of arguments
                    for arg in args {
                        self.type_check_expr(arg);
                    }
                    Type::Int
                }
                InASTExpr::Binary(op, left, right) => {
                    match op {
                        BinaryOp::Set | BinaryOp::SetAdd | BinaryOp::SetSub => {
                            // when setting, left side has to be lvalue
                            if let Some(msg) = match left.0 {
                                InASTExpr::Int(_) => Some("Int literal"),
                                InASTExpr::VarName(name) => {
                                    // make sure that var name exists
                                    self.possibly_report_variable_nonexistance(name, left.1);
                                    None
                                }
                                InASTExpr::FunctionCall(_, _) => Some("function call"),
                                InASTExpr::Binary(op2, _, _) => {
                                    let string: &'static str =
                                        format!("binary operator {:?}", op2).leak();
                                    Some(string)
                                }
                            } {
                                self.report_error_on_token_idx(
                                    &format!("lvalue to setter `{:?}` can't be: `{}`", op, msg),
                                    left.1,
                                );
                            }

                            // right side has to be Int
                            let right_type = self.type_check_expr(right);
                            if Type::Int != right_type {
                                self.report_error_on_token_idx(
                                    &format!(
                                        "Setter operator {:?} expected it's right side to be of type `{:?}` but received type: `{:?}`!",
                                        op, Type::Int, right_type
                                    ),
                                    expr.1,
                                );
                            }
                            Type::Unit
                        }
                        BinaryOp::Equals
                        | BinaryOp::Less
                        | BinaryOp::Greater
                        | BinaryOp::Add
                        | BinaryOp::Sub
                        | BinaryOp::Multiply => {
                            let left_type = self.type_check_expr(left);
                            let right_type = self.type_check_expr(right);
                            if left_type != right_type {
                                // TODO: print types without debug print
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
        current_vars: HashSet::new(),

        ident_to_name,
        source,
        file_name,
        token_idx_to_char_range,
    };
    s.type_check_scope(body, 0);
}
