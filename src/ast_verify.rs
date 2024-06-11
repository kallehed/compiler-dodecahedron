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
    struct Scope {
        vars: Vec<IdentIdx>,
        returns: bool,
        /// if a normal block {} returns, the outer block returns as well
        propogates_return: bool,
        /// is function body
        must_return: bool,
    }
    struct State<'b> {
        /// function ident and nr of parameters
        //function_calls: Vec<&'b ASTExpr>, // not used?
        /// O(1) local var exists checker
        vars: HashSet<IdentIdx>,
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
    #[derive(Debug, Clone, Copy, Eq, PartialEq)]
    enum Type {
        /// Returned by expressions such as x = 3;
        Unit,
        /// standard type for language currently
        Int,
    }
    let mut s = State {
        vars: HashSet::new(),
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

    enum StackItem {
        /// result of binop with variable, or from function call (can't evaulate those here (yet))
        Unknown,
        Int(i64),
        /// from variable, or argument to function
        Var(IdentIdx),
        Unit,
    }
    impl StackItem {
        fn as_var(self) -> IdentIdx {
            match self {
                StackItem::Var(i) => i,
                _ => unreachable!(),
            }
        }
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
            self.scopes.last_mut().unwrap().vars.push(name);
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
                vars: Vec::new(),
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
        /// what it does:
        /// (*) signifies that it can ONLY be done after parsing, or in pass after parsing
        ///
        /// - make sure setters like =,+=,-= are only called with idents as left arg
        /// - make sure all functions return values across all control flow paths
        /// - propogate constants through expressions like 1+2 to 3
        /// *- make sure function calls provide correct number of arguments
        /// *- calls to nonexistent functions caught
        /// - variables are not used before declaration
        /// - variables are not declared more than once
        /// - possibly(?) verify that you aren't doing things like x = (x = 3);
        fn verify(&mut self) {
            // for each scope, we have a vec cataloging the variables that exist, makes poping easier
            loop {
                match self.eat() {
                    Soken::EndStat => {
                        self.sexpect_clear(1); // throw away 1 stack item
                    }
                    Soken::Return => {
                        self.sexpect_clear(1); // throw away 1 stack item
                                               // signal that this scope returns
                        self.scopes.last_mut().unwrap().returns = true;
                    }
                    Soken::Int(e) => self.spush(StackItem::Int(e)),
                    // lazy checking for if variable is declared in binop -> bc parsing function declaration, don't know if best way
                    Soken::Var(e) => self.spush(StackItem::Var(e)),

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
                        self.spush(StackItem::Unknown); // add unknown stack element, because we don't know what the function returns
                    }
                    // make sure that setters have a left l-value,
                    // if regular binop, constant propogate ints
                    Soken::Binop(binop) => {
                        // self.sexpect(2, "binop needs two values");
                        let right = self.spop();
                        let left = self.spop();
                        fn check_declared(s: &mut State, it: &(StackItem, SokIdx)) {
                            if let StackItem::Var(e) = it.0 {
                                if !s.vars.contains(&e) {
                                    s.report_error_on_token_idx(
                                        "Variable used before declaration",
                                        it.1,
                                    );
                                }
                            }
                        }
                        check_declared(self, &left);
                        check_declared(self, &right);
                        use BinaryOp as B;
                        match binop {
                            B::SetAdd | B::SetSub | B::Set => {
                                // `l` has to be ident
                                if let StackItem::Var(_) = left.0 {
                                    self.spush(StackItem::Unit); // setting returns Unit
                                } else {
                                    self.report_error( "Left hand side of setter can't be expression, must be variable name")
                                }
                            }
                            B::Eql | B::Les | B::Mor | B::Add | B::Sub | B::Mul => {
                                // constant propogation
                                if let (StackItem::Int(l), StackItem::Int(r)) = (left.0, right.0) {
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
                                    self.spush(StackItem::Int(res));
                                } else {
                                    // one is either unknown or variable -> Unknown
                                    self.spush(StackItem::Unknown);
                                }
                            }
                        }
                    }
                    Soken::If => {
                        // TODO: check type of arg, can't be unit
                        self.sexpect_clear(1);
                        self.create_scope(false, false); // don't know if reached
                    }
                    Soken::While => {
                        self.sclear(); // remove COND (1 element)
                        self.create_scope(false, false); // don't know if reached
                    }
                    Soken::ScopeStart => self.create_scope(true, false), // basic scope
                    // remove the variables that were in the scope from the global vars
                    Soken::ScopeEnd => {
                        let scope = self.scopes.last().unwrap();
                        for var in scope.vars.iter() {
                            assert!(self.vars.remove(var));
                        }
                        if scope.must_return && !scope.returns {
                            // BAD! Can't just end while not having returned anything!
                            self.report_error("Function did not return");
                        }
                        let outer_scope_returns = scope.propogates_return && scope.returns;
                        self.scopes.pop();

                        if self.scopes.len() > 0 {
                            // if at function
                            self.scopes.last_mut().unwrap().returns |= outer_scope_returns;
                        }
                    }
                    Soken::Nil => {
                        unreachable!("ast won't contain nil after parsing, so impossible")
                    }
                }
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
