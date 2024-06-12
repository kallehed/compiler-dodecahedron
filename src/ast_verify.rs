use crate::parser::Soken;
use std::collections::{HashMap, HashSet};

use crate::{parser::BinaryOp, IdentIdx};

// check correctness of program + constant folding
pub fn run<'a>(
    sokens: &'a mut [Soken],
    origins: &'a [usize],
    ident_idx_to_string: &[&'static str],
    functions: &HashMap<IdentIdx, u16>,
    source: &'a str,
    file_name: &'a str,
    token_idx_to_char_range: &[(usize, usize)],
) {
    /// scope {} data
    struct Scope {
        // how many vars were created in this scope?
        vars: usize,
        returns: bool,
        /// if a normal block {} returns, the outer block returns as well
        propogates_return: bool,
        /// is function body
        must_return: bool,
    }
    /// Why even let the Stack hold both (StackItem, SokIdx), the stackItem says if it's a literal,mixint,var or Unit
    /// The SokIdx says where it was found. BUT: Instead of looking at the StackItem, we can simply index Soken's
    /// with the SokIdx, and we will get what it was (Any Soken) (Int,Var,Func), we can also use Nil to represent a
    /// combined value of Var and Int.
    /// well, no, we don't just want to store indicies into sokens, because then we don't get any type safety on
    /// our stack values, and the only things on our stack is vars, literals and so on.
    /// a better way would be to NOT store literals inline, but use the soken index to get the literal if we want
    struct State<'b> {
        /// function ident and nr of parameters
        //function_calls: Vec<&'b ASTExpr>, // not used?
        /// O(1) local var exists checker
        vars: HashSet<IdentIdx>,
        // each scope holds how many vars it creates, they are added here and removed from self.vars after scope ends
        ordered_vars: Vec<IdentIdx>,
        scopes: Vec<Scope>,
        /// holds values, args and stuff, with their soken idx
        stack: Vec<(StackItem, SokIdx)>,
        sokens: &'b mut [Soken],
        /// map from index of soken to it's token index in source file
        origins: &'b [usize],
        si: SokIdx,

        // constant
        functions: &'b HashMap<IdentIdx, u16>,
        ident_idx_to_string: &'b [&'static str],
        source: &'b str,
        file_name: &'b str,
        token_idx_to_char_range: &'b [(usize, usize)],
    }
    let mut s = State {
        vars: HashSet::new(),
        ordered_vars: Vec::new(),
        scopes: Vec::new(),
        stack: Vec::new(),
        sokens,
        origins,
        si: SokIdx(usize::MAX), // so it wraps to 0 at start, YES HANDLED CORRECTLY

        functions,
        ident_idx_to_string,
        source,
        file_name,
        token_idx_to_char_range,
    };
    s.verify();

    /// maybe better way, is: It could either be a literal, or it could be something unknown with a type
    enum StackItem {
        /// int literal 34, 54, 21, use SokIdx to get what it is if you want
        /// common case is not to look at int literals, so use array access for that.
        LitInt,
        /// from variable, or argument to function, OR result of variable addition or whatever x + y, x+1
        UnkownInt,
        Unit,
    }
    #[derive(Copy, Clone, Debug)]
    struct SokIdx(usize);

    impl State<'_> {
        /// add var, also send token index of soken of var, uses implicit si
        fn add_var(&mut self, name: IdentIdx) {
            if !self.vars.insert(name) {
                self.report_error("Variable has already been declared!");
            }
            // if global scope contains it, this can't (can't assert bc Vector::push)
            self.scopes.last_mut().unwrap().vars += 1;
            self.ordered_vars.push(name);
        }
        /// stack pop, get item, and where it originated from sokens
        fn spop(&mut self) -> (StackItem, SokIdx) {
            self.stack.pop().unwrap()
        }
        /// stack push item, including `i`, soken index it comes from
        fn spush(&mut self, it: StackItem) {
            self.stack.push((it, self.si));
        }
        fn sclear(&mut self) {
            self.stack.clear();
        }
        fn create_scope(&mut self, propogates_return: bool, must_return: bool) {
            self.scopes.push(Scope {
                returns: false,
                vars: 0,
                propogates_return,
                must_return,
            });
        }
        fn eat(&mut self) -> Soken {
            self.si.0 = self.si.0.wrapping_add(1);
            self.sokens[self.si.0]
        }
        /// internal logic assertion on nbr of items on stack
        fn sexpect(&mut self, items: usize) {
            if self.stack.len() != items {
                println!();
                self.report_error(&format!(
                    "got {} stackitems, but wants {}",
                    self.stack.len(),
                    items
                ));
            }
        }
        fn sexpect_clear(&mut self, items: usize) {
            self.sexpect(items);
            self.sclear();
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
        /// TODO-FIX: can't to return x = 3;
        fn verify(&mut self) {
            // for each scope, we have a vec cataloging the variables that exist, makes poping easier
            loop {
                match self.eat() {
                    Soken::EndStat => {
                        self.sexpect_clear(1); // throw away 1 stack item
                    }
                    // TODO: expr has to be int
                    Soken::Return => {
                        self.sexpect(1);
                        let ret_val = self.spop();
                        if let StackItem::Unit = ret_val.0 {
                            self.report_error("Can't return Unit");
                        }
                        // signal that this scope returns
                        self.scopes.last_mut().unwrap().returns = true;
                    }
                    Soken::CreateVar(name) => {
                        self.sexpect(0);
                        self.add_var(name);
                    }
                    // basically just create arg variables
                    // TODO: maybe store name ident in scope so we can print it later if function doesn't return?
                    Soken::FuncDef(name) => {
                        self.sexpect(0);
                        self.create_scope(false, true); // false bc outer scope is weird
                        let args = *self.functions.get(&name).unwrap();
                        // add to scope, must not shadow, TODO
                        for _ in 0..args {
                            match self.eat() {
                                Soken::Var(arg) => {
                                    self.add_var(arg);
                                }
                                _ => unreachable!(),
                            }
                        }
                    }
                    // TODO: check type of arg, can't be unit
                    Soken::If => {
                        self.sexpect_clear(1);
                        self.create_scope(false, false); // don't know if reached
                    }
                    Soken::While => {
                        self.sexpect_clear(1);
                        self.create_scope(false, false); // don't know if reached
                    }
                    Soken::ScopeStart => {
                        self.sexpect(0);
                        self.create_scope(true, false);
                    } // basic scope
                    // remove the variables that were in the scope from the global vars
                    Soken::ScopeEnd => {
                        self.sexpect(0);
                        let scope = self.scopes.last().unwrap();
                        for _ in 0..scope.vars {
                            assert!(self.vars.remove(&self.ordered_vars.pop().unwrap()));
                        }
                        if scope.must_return && !scope.returns {
                            // BAD! Can't just end while not having returned anything!
                            self.report_error("Function did not return");
                        }
                        let outer_scope_returns = scope.propogates_return && scope.returns;
                        self.scopes.pop();

                        if !self.scopes.is_empty() {
                            // if at function
                            self.scopes.last_mut().unwrap().returns |= outer_scope_returns;
                        }
                    }
                    // HERE BEGINS EXPR SOKENS
                    Soken::Int(_) => self.spush(StackItem::LitInt),
                    // Check that variable has been declared. This works for the FN_DEF case, bc args are afterwards
                    Soken::Var(e) => {
                        if !self.vars.contains(&e) {
                            self.report_error("Variable used before declaration");
                        }
                        self.spush(StackItem::UnkownInt);
                    }

                    // CALL to ALREADY EXISTING function
                    Soken::FuncCall(name, supposed_args) => {
                        // make sure function called with correct nr of arguments
                        if let Some(&args) = self.functions.get(&name) {
                            if args != supposed_args {
                                self.report_error(&format!(
                                    "Function should be called with {} arg(s), got {}",
                                    args, supposed_args
                                ));
                            }
                        } else {
                            self.report_error("Function does not exist");
                        }
                        self.sclear(); // drop elements of stack
                        self.spush(StackItem::UnkownInt); // add unknown stack element, because we don't know what the function returns
                    }
                    // make sure that setters have a left l-value,
                    // if regular binop, constant propogate ints
                    Soken::Binop(binop) => {
                        // self.sexpect(2, "binop needs two values");
                        let right = self.spop();
                        let left = self.spop();
                        // TODO: DRY the code
                        match left.0 {
                            StackItem::LitInt | StackItem::UnkownInt => (),
                            StackItem::Unit => self.report_error("Can't do binop on Unit"),
                        }
                        match right.0 {
                            StackItem::LitInt | StackItem::UnkownInt => (),
                            StackItem::Unit => self.report_error("Can't do binop on Unit"),
                        }
                        use BinaryOp as B;
                        match binop {
                            B::SetAdd | B::SetSub | B::Set => {
                                // `left` has to be ident
                                if let StackItem::UnkownInt = left.0 {
                                    self.spush(StackItem::Unit); // setting returns Unit
                                } else {
                                    self.report_error( "Left hand side of setter can't be expression, must be variable name")
                                }
                            }
                            B::Eql | B::Les | B::Mor | B::Add | B::Sub | B::Mul => {
                                // constant propogation
                                if let (StackItem::LitInt, StackItem::LitInt) = (left.0, right.0) {
                                    // get values
                                    let l = match self.sokens[left.1 .0] {
                                        Soken::Int(e) => e,
                                        _ => unreachable!(),
                                    };
                                    let r = match self.sokens[right.1 .0] {
                                        Soken::Int(e) => e,
                                        _ => unreachable!(),
                                    };
                                    // bc rust semantics we do wrapping_add etc
                                    let res = match binop {
                                        B::Eql => (l == r) as i64,
                                        B::Les => (l < r) as i64,
                                        B::Mor => (l > r) as i64,
                                        B::Add => l.wrapping_add(r),
                                        B::Sub => l.wrapping_sub(r),
                                        B::Mul => l.wrapping_mul(r),
                                        _ => unreachable!(),
                                    };
                                    self.sokens[left.1 .0] = Soken::Nil;
                                    self.sokens[right.1 .0] = Soken::Nil;
                                    self.sokens[self.si.0] = Soken::Int(res); // propogation
                                    self.spush(StackItem::LitInt);
                                } else {
                                    self.spush(StackItem::UnkownInt); // one is either unknown or variable -> Var
                                }
                            }
                        }
                    }
                    Soken::Nil => {
                        unreachable!("ast won't contain nil after parsing, so impossible")
                    }
                }
                // this is WEIRD, but bc we start at usize::MAX for `si` we have to do this at end
                if self.si.0 + 1 >= self.sokens.len() {
                    break;
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
}
