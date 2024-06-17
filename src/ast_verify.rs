use crate::parser::Soken;
use std::collections::{HashMap, HashSet};

use crate::{parser::BinaryOp, IdentIdx, Int};

use crate::lexer::IntStor;

/// Could either be a literal, or it could be something unknown with a type
enum StackItem {
    /// int literal 34, 54, 21, use SokIdx to get what it is if you want from int_stor
    /// common case is not to look at int literals, so use array access for that.
    LitInt,
    /// result of variable addition or whatever x + y, x+1
    UnkownInt,
    /// from variable (IMPORTANT: Used when doing x += 3), we need this, also for parameter to func_def
    Var,
    Unit,
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
        si: SokIdx(usize::MAX), // so it wraps to 0 at start, YES HANDLED CORRECTLY

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
        self.si.0 = self.si.0.wrapping_add(1);
        self.sokens[self.si.0]
    }

    /// add var, also send token index of soken of var, uses implicit si
    fn add_var(&mut self, name: IdentIdx) {
        if !self.vars.insert(name) {
            self.report_error("Variable has already been declared!");
        }
        // if global scope contains it, this can't (can't assert bc Vector::push)
        self.scopes.last_mut().unwrap().vars += 1;
        self.ordered_vars.push(name);
    }
    /// What it does:
    /// (*) signifies that it can ONLY be done after parsing, or in pass after parsing
    ///
    /// - make sure setters like =,+=,-= are only called with variables as left arg
    /// - make sure all functions return values across all control flow paths
    /// - propogate constants through expressions like 1+2 to 3
    /// *- make sure function calls provide correct number of arguments
    /// *- calls to nonexistent functions caught
    /// - variables not (used before declaration, declared more than once)
    /// - verify that you aren't doing things like x = (x = 3);
    fn verify(&mut self) {
        // for each scope, we have a vec cataloging the variables that exist, makes poping easier
        loop {
            use Soken as S;
            match self.eat() {
                // Could be better abstracted, to be able to receive items or not care
                S::Return => {
                    let res = self.verify_expr();
                    self.is_int(res);
                    returns = true;
                }
                S::CreateVar(name) => {
                    self.add_var(name);
                }
                // basically just create arg variables
                // TODO: maybe store name ident in scope so we can print it later if function doesn't return?
                S::FuncDef(name) => {
                    self.sexpect(0);
                    self.create_scope(false, true, name); // false bc outer scope is weird
                    let args = *self.functions.get(&name).unwrap();
                    for _ in 0..args {
                        match self.eat() {
                            Soken::Var(arg) => {
                                self.add_var(arg);
                            }
                            _ => unreachable!(),
                        }
                    }
                }
                S::If => {
                    let typ = self.verify_expr();
                    self.is_int(typ);
                    let asd = self.eat();
                    assert_starblock(asd);
                }
                S::Else => {
                    self.sexpect(0);
                    self.create_scope(false, false, 0);
                }
                S::While => {
                    self.sexpect(1);
                    self.sexpect_int("While condition must be Int");
                    self.create_scope(false, false, 0); // don't know if reached
                }
                S::ScopeStart => {
                    self.sexpect(0);
                    self.create_scope(true, false, 0);
                } // basic scope
                // remove the variables that were in the scope from the global vars
                S::ScopeEnd => {
                    self.sexpect(0);
                    let scope = self.scopes.last().unwrap();
                    for _ in 0..scope.vars {
                        assert!(self.vars.remove(&self.ordered_vars.pop().unwrap()));
                    }
                    if scope.must_return && !scope.returns {
                        self.report_error(&format!(
                            "Function `{}` did not return",
                            self.ident_idx_to_string[scope.name as usize]
                        ));
                    }
                    let outer_scope_returns = scope.propogates_return && scope.returns;
                    self.scopes.pop();

                    if !self.scopes.is_empty() {
                        // if at function
                        self.scopes.last_mut().unwrap().returns |= outer_scope_returns;
                    }
                }
                _ => {
                    panic!("wrong Soken in ast_verify, probably got Int even though at statement level");
                }
                S::Nil => {
                    unreachable!("ast won't contain nil after parsing, so impossible");
                }
            }
            // this is WEIRD, but bc we start at usize::MAX for `si` we have to do this at end
            if self.si.0 + 1 >= self.sokens.len() {
                break;
            }
        }
    }

    // HERE BEGINS EXPR SOKENS
    fn verify_expr(&mut self) -> StackItem {
        use Soken as S;
        match self.eat() {
            S::Int(_) => self.spush(StackItem::LitInt),
            // Check that variable has been declared. This works for the FN_DEF case, bc args are afterwards
            S::Var(e) => {
                if !self.vars.contains(&e) {
                    self.report_error("Variable used before declaration");
                }
                self.spush(StackItem::Var);
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
                    self.spop();
                }
                self.spush(StackItem::UnkownInt); // add unknown stack element, because we don't know what the function returns
            }
            // make sure that setters have a left l-value,
            // if regular binop, constant propogate ints
            S::Binop(binop) => {
                // self.sexpect(2, "binop needs two values");
                let right = self.sexpect_int("Can't do binop on Unit");
                let left = self.sexpect_int("Can't do binop on Unit");

                use BinaryOp as B;
                match binop {
                    B::SetAdd | B::SetSub | B::Set => {
                        // `left` has to be ident, ERROR
                        if !matches!(left.0, StackItem::Var) {
                            self.report_error( "Left hand side of setter can't be expression, must be variable name")
                        }
                        self.spush(StackItem::Unit); // setting returns Unit
                    }
                    B::Eql | B::Les | B::Mor | B::Add | B::Sub | B::Mul => {
                        // constant propogation
                        if let (StackItem::LitInt, StackItem::LitInt) = (left.0, right.0) {
                            // get values
                            let l = match self.sokens[left.1 .0] {
                                Soken::Int(e) => self.int_stor.get(e),
                                _ => unreachable!(),
                            };
                            let r = match self.sokens[right.1 .0] {
                                Soken::Int(e) => self.int_stor.get(e),
                                _ => unreachable!(),
                            };
                            // bc rust semantics we do wrapping_add etc
                            let res = match binop {
                                B::Eql => (l == r) as Int,
                                B::Les => (l < r) as Int,
                                B::Mor => (l > r) as Int,
                                B::Add => l.wrapping_add(r),
                                B::Sub => l.wrapping_sub(r),
                                B::Mul => l.wrapping_mul(r),
                                _ => unreachable!(),
                            };
                            let new_idx = self.int_stor.insert_num_get_idx(res);

                            self.sokens[left.1 .0] = Soken::Nil;
                            self.sokens[right.1 .0] = Soken::Nil;
                            self.sokens[self.si.0] = Soken::Int(new_idx); // propogation
                            self.spush(StackItem::LitInt);
                        } else {
                            self.spush(StackItem::UnkownInt); // one is either unknown or variable -> Var
                        }
                    }
                }
            }
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
