use crate::parser::Soken;
use std::collections::{HashMap, HashSet};

use crate::{parser::BinaryOp, IdentIdx, Int};

#[derive(Copy, Clone, Debug)]
struct SokIdx(usize);
struct State<'b> {
    /// holds indexes into buffer and their lengths
    stack: Vec<(usize, usize)>,
    sokens: &'b [Soken],
    si: SokIdx,

    c_declarations: String,
    c_code: String,
    /// holdds intermediate expression strings
    buffer: String,

    // constant
    int_storage: &'b Vec<Int>,
    functions: &'b HashMap<IdentIdx, u16>,
    ident_idx_to_string: &'b [&'static str],
    source: &'b str,
    file_name: &'b str,
    token_idx_to_char_range: &'b [(usize, usize)],
}

// check correctness of program + constant folding
pub fn run<'a>(
    sokens: &'a mut [Soken],
    ident_idx_to_string: &[&'static str],
    functions: &HashMap<IdentIdx, u16>,
    source: &'a str,
    file_name: &'a str,
    token_idx_to_char_range: &[(usize, usize)],
    int_storage: &mut Vec<Int>,
) {
    let mut s = State {
        stack: Vec::new(),
        sokens,
        si: SokIdx(usize::MAX), // so it wraps to 0 at start, YES HANDLED CORRECTLY

        c_declarations: String::new(),
        c_code: String::new(),
        buffer: String::new(),

        int_storage,
        functions,
        ident_idx_to_string,
        source,
        file_name,
        token_idx_to_char_range,
    };
    s.verify();
}

impl State<'_> {
    /// stack pop, get item, and where it originated from sokens
    fn spop(&mut self) -> &str {
        self.stack.pop().unwrap()
    }
    /// stack push item, including `i`, soken index it comes from
    fn spush(&mut self, it: &str) {
        self.stack.push(it);
    }
    fn sclear(&mut self) {
        self.stack.clear();
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
    fn print(&mut self, s: &str) {
        self.c_code.push_str(s);
    }
    fn print_first_on_stack(&mut self) {
        let e = self.stack.pop().unwrap();
        self.print(e);
    }
    fn var_name(&self, name: IdentIdx) -> String {
        format!("var{}", name)
    }
    fn func_name(&self, name: IdentIdx) -> String {
        format!("func{}", name)
    }
    fn verify(&mut self) {
        // for each scope, we have a vec cataloging the variables that exist, makes poping easier
        loop {
            match self.eat() {
                Soken::EndStat => {
                    self.sexpect(0);
                    self.print_first_on_stack();
                    self.print(";");
                }
                Soken::Return => {
                    self.sexpect(1);
                    self.print("return ");
                    self.print_first_on_stack();
                    self.print(";");
                }
                // initilize everything to 0
                Soken::CreateVar(name) => {
                    self.sexpect(0);
                    self.print(&format!("long {}=0;", self.var_name(name)));
                }
                Soken::FuncDef(name) => {
                    self.sexpect(0);
                    self.print(&format!("long {}(", self.func_name(name)));
                    let args = *self.functions.get(&name).unwrap();
                    for _ in 0..args {
                        match self.eat() {
                            Soken::Var(arg) => {
                                self.print(&format!("{},", self.var_name(arg)));
                            }
                            _ => unreachable!(),
                        }
                    }
                    self.print("){");
                }
                // TODO: check type of arg, can't be unit
                Soken::If => {
                    self.sexpect(1);
                    self.print("if (");
                    self.print_first_on_stack();
                    self.print("){");
                }
                Soken::While => {
                    self.sexpect(1);
                    self.print("while (");
                    self.print_first_on_stack();
                    self.print("){");
                }
                Soken::ScopeStart => {
                    self.sexpect(0);
                    self.print("{");
                } // basic scope
                // remove the variables that were in the scope from the global vars
                Soken::ScopeEnd => {
                    self.sexpect(0);
                    self.print("}");
                }
                // HERE BEGINS EXPR SOKENS
                Soken::Int(intstor) => {
                    let theint = self.int_storage[intstor as usize];
                    let string_version = theint.to_string();
                    let start = self.buffer.len();
                    let len = string_version.len();
                    self.buffer.push_str(&string_version);
                    self.stack.push((start, len));
                }
                // Check that variable has been declared. This works for the FN_DEF case, bc args are afterwards
                Soken::Var(e) => {
                    if !self.vars.contains(&e) {
                        self.report_error("Variable used before declaration");
                    }
                    self.spush(StackItem::Var);
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
                        StackItem::LitInt | StackItem::UnkownInt | StackItem::Var => (),
                        StackItem::Unit => self.report_error("Can't do binop on Unit"),
                    }
                    match right.0 {
                        StackItem::LitInt | StackItem::UnkownInt | StackItem::Var => (),
                        StackItem::Unit => self.report_error("Can't do binop on Unit"),
                    }
                    use BinaryOp as B;
                    match binop {
                        B::SetAdd | B::SetSub | B::Set => {
                            // `left` has to be ident, ERROR
                            if let StackItem::Var = left.0 {
                                self.spush(StackItem::Unit); // setting returns Unit
                            } else {
                                self.report_error( "Left hand side of setter can't be expression, must be variable name")
                            }
                        }
                        B::Eql | B::Les | B::Mor | B::Add | B::Sub | B::Mul => {
                            // constant propogation
                            if let (StackItem::LitInt, StackItem::LitInt) = (left.0, right.0) {
                                // get values
                                let l_intstor = match self.sokens[left.1 .0] {
                                    Soken::Int(e) => e,
                                    _ => unreachable!(),
                                };
                                let r_intstor = match self.sokens[right.1 .0] {
                                    Soken::Int(e) => e,
                                    _ => unreachable!(),
                                };
                                let l = self.int_storage[l_intstor as usize];
                                let r = self.int_storage[r_intstor as usize];
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
                                // replace left int_storage with new value
                                self.int_storage[l_intstor as usize] = res;
                                self.sokens[self.si.0] = Soken::Int(l_intstor); // propogation
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
