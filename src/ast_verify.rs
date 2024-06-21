use crate::parser::Soken;
use std::collections::{HashMap, HashSet};

use crate::{parser::BinaryOp, IdentIdx, Int};

use crate::lexer::IntStor;

/// Could either be a literal, or it could be something unknown with a type
#[derive(Clone, Copy)]
enum Expr {
    /// int literal 34, 54, 21, use SokIdx to get what it is if you want from int_stor
    /// common case is not to look at int literals, so use array access for that.
    LitInt,
    /// result of variable addition or whatever x + y, x+1
    UnkownInt,
    /// from variable (IMPORTANT: Used when doing x += 3), we need this, also for parameter to func_def
    Var,
    Unit,
}
struct Scope {
    returns: bool,
    local_vars: u16,
}

#[derive(Copy, Clone, Debug)]
struct SokIdx(usize);
struct State<'b> {
    /// function ident and nr of parameters
    //function_calls: Vec<&'b ASTExpr>, // not used?
    /// O(1) local var exists checker
    vars: HashSet<IdentIdx>,
    // each scope holds how many vars it creates, they are added here and removed from self.vars after scope ends
    ordered_vars: Vec<IdentIdx>,
    sokens: &'b mut [Soken],
    /// map from index of soken to it's token index in source file
    origins: &'b [usize],
    si: SokIdx,

    /// expr stack with corresponding SokIdx
    exprs: Vec<(Expr, SokIdx)>,
    scopes: Vec<Scope>,

    // not constant, because we can add new ints when constant propogation
    int_stor: &'b mut IntStor,

    // constant
    functions: &'b HashMap<IdentIdx, u16>,
    ident_idx_to_string: &'b [&'static str],
    source: &'b str,
    file_name: &'b str,
    token_idx_to_char_range: &'b [(usize, usize)],
}

// check correctness of program + constant folding
pub fn run<'a>(
    sokens: &'a mut [Soken],
    origins: &'a [usize],
    ident_idx_to_string: &[&'static str],
    functions: &HashMap<IdentIdx, u16>,
    source: &'a str,
    file_name: &'a str,
    token_idx_to_char_range: &[(usize, usize)],
    int_stor: &mut IntStor,
) {
    let mut s = State {
        vars: HashSet::new(),
        ordered_vars: Vec::new(),
        sokens,
        origins,
        si: SokIdx(0), // so it wraps to 0 at start, YES HANDLED CORRECTLY
        exprs: Vec::new(),
        scopes: Vec::new(),

        int_stor,

        functions,
        ident_idx_to_string,
        source,
        file_name,
        token_idx_to_char_range,
    };
    s.verify();
}

impl State<'_> {
    fn eat(&mut self) -> Soken {
        let s = self.sokens[self.si.0];
        self.si.0 += 1;
        s
    }
    /// add var, also send token index of soken of var, uses implicit si
    fn add_var(&mut self, name: IdentIdx) {
        if !self.vars.insert(name) {
            self.report_error("Variable has already been declared!");
        }
        // if global scope contains it, this can't (can't assert bc Vector::push)
        self.scopes.last_mut().unwrap().local_vars += 1;
        self.ordered_vars.push(name);
    }
    /// WARNING: only use if the latest self.eat() you called is associated with the expr you are pushing, bc we will also push your self.si
    fn epush(&mut self, e: Expr) {
        // -1 because at .eat() we go one forward
        self.exprs.push((e, SokIdx(self.si.0 - 1)));
    }
    fn epop(&mut self) -> (Expr, SokIdx) {
        self.exprs.pop().unwrap()
    }

    fn require_int(&mut self, expr: Expr, msg: &str) {
        if let Expr::Unit = expr {
            self.report_error(msg);
        }
    }
    fn verify(&mut self) {
        loop {
            self.verify2();
            if self.si.0 >= self.sokens.len() {
                break;
            }
        }
    }
    /// What it does: (it assumes the Soken's are in right order though)
    /// (*) signifies that it can ONLY be done after parsing, or in pass after parsing
    ///
    /// - make sure setters like =,+=,-= are only called with variables as left arg
    /// - make sure all functions return values across all control flow paths
    /// // - propogate constants through expressions like 1+2 to 3
    /// *- make sure function calls provide correct number of arguments
    /// *- calls to nonexistent functions caught
    /// - variables not (used before declaration | declared more than once)
    /// - verify that you aren't doing things like x = (x = 3);
    /// - turn RValue into LValue, for better experience in ir_gen.rs later
    fn verify2(&mut self) {
        use Soken as S;
        match self.eat() {
            S::Return => {
                // take expression, must be int though
                let (expr, _) = self.epop();
                self.require_int(expr, "Can only return int");
                self.scopes.last_mut().unwrap().returns = true;
            }
            S::CreateVar(name) => {
                self.add_var(name);
            }
            // basically just create arg variables
            // TODO: maybe store name ident in scope so we can print it later if function doesn't return?
            S::FuncDef(name) => {
                self.scopes.push(Scope {
                    returns: false,
                    local_vars: 0,
                });
                let args = *self.functions.get(&name).unwrap();
                for _ in 0..args {
                    match self.eat() {
                        Soken::RVar(arg) => {
                            self.add_var(arg);
                        }
                        _ => unreachable!(),
                    }
                }
            }
            S::If => {
                let (cond, _) = self.epop();
                let else_part = self.scopes.pop().unwrap();
                let if_part = self.scopes.pop().unwrap();
                self.require_int(cond, "If condition must be int");
                self.scopes.last_mut().unwrap().returns |= if_part.returns & else_part.returns;
            }
            S::While => {
                let (cond, _) = self.epop();
                let _scope = self.scopes.pop().unwrap();
                self.require_int(cond, "While condition must be int");
            }
            S::StartScope => {
                self.scopes.push(Scope {
                    returns: false,
                    local_vars: 0,
                });
            }
            // remove variable names, keep scope on stack though
            S::EndScope => {
                let scope = self.scopes.last().unwrap();
                for _ in 0..scope.local_vars {
                    assert!(self.vars.remove(&self.ordered_vars.pop().unwrap()));
                }
            }
            S::DropScope => {
                let basic_scope = self.scopes.pop().unwrap();
                self.scopes.last_mut().unwrap().returns |= basic_scope.returns;
            }
            S::EndFuncDef(_) => {
                // compile error if we did not return
                let func_scope = self.scopes.pop().unwrap();
                if !func_scope.returns {
                    self.report_error("Function must return across all control flow paths");
                }
            }

            // EXPRESSION SOKENS BEGIN HERE!!!!!!!!!!
            S::Int(_) => self.epush(Expr::LitInt),
            // Check that variable has been declared. This works for the FN_DEF case, bc args are afterwards
            S::RVar(e) => {
                if !self.vars.contains(&e) {
                    self.report_error("Variable used before declaration");
                }
                self.epush(Expr::Var);
            }

            // CALL to ALREADY EXISTING function
            S::FuncCall(name, supposed_args) => {
                // make sure function called with correct nr of arguments
                if let Some(&args) = self.functions.get(&name) {
                    if args != supposed_args {
                        self.report_error(&format!(
                            "Function should be called with {} arg(s), got {}",
                            args, supposed_args
                        ));
                    }
                } else {
                    self.report_error(&format!(
                        "Function `{}` does not exist",
                        self.ident_idx_to_string[name as usize]
                    ));
                }
                // drop supposed_args amount of arguments
                for _ in 0..supposed_args {
                    self.epop();
                }
                self.epush(Expr::UnkownInt); // add unknown stack element, because we don't know what the function returns
            }
            // make sure that setters have a left l-value,
            // if regular binop, constant propogate ints
            S::Binop(binop) => {
                // self.sexpect(2, "binop needs two values");
                let (right, r_p) = self.epop();
                let (left, l_p) = self.epop();
                self.require_int(right, "Right expr of binop must be int");
                self.require_int(left, "Left expr of binop must be int");
                use BinaryOp as B;
                match binop {
                    B::SetAdd | B::SetSub | B::Set => {
                        // `left` has to be ident, ERROR
                        match left {
                            Expr::Var => {
                                // MODIFICATION!!!!!!!!!
                                match self.sokens[l_p.0] {
                                    S::RVar(name) => {
                                        self.sokens[l_p.0] = Soken::LVar(name);
                                        self.epush(Expr::Unit); // setting returns Unit
                                    }
                                    _ => unreachable!(),
                                }
                            }
                            _ => self.report_error( "Left hand side of setter can't be expression, must be variable name")
                        }
                    }
                    B::Eql | B::Les | B::Mor | B::Add | B::Sub | B::Mul => {
                        self.epush(Expr::UnkownInt); // one is either unknown or variable -> Var
                    }
                }
            }
            S::EndStat => {
                self.exprs.pop();
            }
            S::Nil => {
                unreachable!("ast won't contain nil after parsing, so impossible");
            }
            S::LVar(_) => unreachable!("won't be gen by parser"),
        }
    }

    fn report_error_on_token_idx(&mut self, msg: &str, si: SokIdx) -> ! {
        crate::mark_error_in_source(
            self.source,
            self.file_name,
            msg,
            self.token_idx_to_char_range[self.origins[si.0]],
        );
        std::process::exit(1);
    }
    // implcitily reports on current soken idx
    fn report_error(&mut self, msg: &str) -> ! {
        self.report_error_on_token_idx(msg, self.si);
    }
}
