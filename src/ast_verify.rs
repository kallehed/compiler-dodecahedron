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
        stack: Vec<(StackItem, usize)>,
        sokens: &'b mut [Soken],
        /// map from index of soken to it's token index in source file
        origins: &'b [usize],

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
    enum Returns {
        No,
        Yes,
    }
    let mut s = State {
        vars: HashSet::new(),
        scopes: Vec::new(),
        stack: Vec::new(),
        sokens,
        origins,

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
    }
    impl StackItem {
        fn as_var(self) -> IdentIdx {
            match self {
                StackItem::Var(i) => i,
                _ => unreachable!(),
            }
        }
    }

    impl State<'_> {
        /// add var, also send token index of soken of var
        fn add_var(&mut self, name: IdentIdx, orig: usize) {
            if self.vars.insert(name) {
                self.report_error_on_token_idx("Variable has already been declared!", orig);
            }
            // if global scope contains it, this can't (can't assert bc Vector::push)
            self.scopes.last_mut().unwrap().vars.push(name);
        }
        /// stack pop, get item, and where it originated from sokens
        fn spop(&mut self) -> (StackItem, usize) {
            self.stack.pop().unwrap()
        }
        /// stack push item, including `i`, soken index it comes from
        fn spush(&mut self, it: StackItem, i: usize) {
            self.stack.push((it, i));
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
        // stack expect exactly `items` nr of items
        // TODO unsure if actually needed lol?
        // fn sexpect(&mut self, items: usize, msg: &str) {
        //     if self.stack.len() != items {
        //         self.report_error_on_token_idx(msg, token_idx)
        //     }
        // }
        // stack expect `items` and also clear stack after
        // fn sexpect_clear(&mut self, items: usize, msg: &str) {
        //     self.sexpect(items, msg);
        //     self.sclear();
        // }
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
            for i in 0.. {
                if i >= self.sokens.len() {
                    break;
                }
                let soken = self.sokens[i];
                // token index for current soken
                let orig = self.origins[i];
                match soken {
                    Soken::Return => {
                        // self.sexpect_clear(1, "return needs a value");
                        self.sclear();
                        // signal that this scope returns
                        self.scopes.last_mut().unwrap().returns = true;
                    }
                    Soken::Int(e) => self.spush(StackItem::Int(e), i),
                    Soken::Var(e) => {
                        if !self.vars.contains(&e) {
                            self.report_error_on_token_idx(
                                "Variable used before declaration",
                                orig,
                            );
                        }
                        self.spush(StackItem::Var(e), i);
                    }
                    Soken::CreateVar(name) => self.add_var(name, orig),
                    // basically just create arg variables, then scope will be handled automatically
                    // TODO: maybe store name ident in scope so we can print it later if function doesn't return?
                    Soken::FuncDef(_name, args) => {
                        // we want to take `args` nr of things on the stack, they are identifiers
                        // and we want to introduce them into the scope
                        self.create_scope(false, true); // false bc outer scope is weird
                        for _ in 0..args {
                            let arg = self.spop().0.as_var();
                            // add to scope, must not shadow, TODO
                            self.add_var(arg, orig);
                        }
                        // self.sexpect_clear(0, "unreachable");
                    }
                    // make sure that setters have a left l-value,
                    // if regular binop, constant propogate ints
                    Soken::Binop(binop) => {
                        // self.sexpect(2, "binop needs two values");
                        let (r, r_p) = self.spop();
                        let (l, l_p) = self.spop();
                        use BinaryOp as B;
                        match binop {
                            B::SetAdd | B::SetSub | B::Set => {
                                // `l` has to be ident
                                if let StackItem::Var(_) = l {
                                    // clear stack of `r`
                                    self.sclear();
                                } else {
                                    self.report_error_on_token_idx(
                                        "can only set variables, not exprs",
                                        orig,
                                    )
                                }
                            }
                            B::Eql | B::Les | B::Mor | B::Add | B::Sub | B::Mul => {
                                // constant propogation
                                if let (StackItem::Int(l), StackItem::Int(r)) = (l, r) {
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
                                    self.sokens[l_p] = Soken::Nil;
                                    self.sokens[r_p] = Soken::Nil;
                                    self.sokens[i] = Soken::Int(res); // propogation
                                    self.spush(StackItem::Int(res), i);
                                } else {
                                    // one is either unknown or variable -> Unknown
                                    self.spush(StackItem::Unknown, i);
                                }
                            }
                        }
                    }
                    Soken::FuncCall(name, supposed_args) => {
                        // make sure function called with correct nr of arguments
                        if let Some(&args) = self.functions.get(&name) {
                            if args != supposed_args {
                                self.report_error_on_token_idx(
                                    &format!(
                                        "Function should be called with {} args, got {}",
                                        args, supposed_args
                                    ),
                                    orig,
                                );
                            }
                        } else {
                            self.report_error_on_token_idx("Function called does not exist", orig);
                        }
                        self.sclear(); // drop elements of stack
                        self.spush(StackItem::Unknown, orig); // add unknown stack element, because we don't know what the function returns
                    }
                    Soken::If => {
                        self.sclear(); // remove COND (1 element)
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
                            self.report_error_on_token_idx("Function did not return", orig);
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
    }
}
