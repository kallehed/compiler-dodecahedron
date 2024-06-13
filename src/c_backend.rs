use crate::parser::Soken;
use std::collections::{HashMap, HashSet};

use crate::{parser::BinaryOp, IdentIdx, Int};

#[derive(Clone, Copy)]
enum StackItem {
    /// index into int storage
    Int(u16),
    /// Variable name
    Var(IdentIdx),
    /// binop, index of first arg, second will be left neighbor of first
    Binop(BinaryOp, u16),
    /// fn name and where the args start (then go to the left to get args in right order)
    /// you can get nr of args from the name
    FnCall(IdentIdx, u16),
}

#[derive(Copy, Clone, Debug)]
struct SokIdx(usize);
struct State<'b> {
    /// holds values that the stack refers to
    refer_exprs: Vec<StackItem>,
    /// holds values that soon will be placed on `refer_exprs`
    stack: Vec<StackItem>,
    sokens: &'b [Soken],
    si: SokIdx,

    c_declarations: String,
    c_code: String,

    // constant
    int_storage: &'b [Int],
    functions: &'b HashMap<IdentIdx, u16>,
    ident_idx_to_string: &'b [&'static str],
}

// check correctness of program + constant folding
pub fn to_c_code<'a>(
    sokens: &'a [Soken],
    ident_idx_to_string: &[&'static str],
    functions: &HashMap<IdentIdx, u16>,
    int_storage: &[Int],
) -> String {
    let mut s = State {
        refer_exprs: Vec::new(),
        stack: Vec::new(),
        sokens,
        si: SokIdx(usize::MAX), // so it wraps to 0 at start, YES HANDLED CORRECTLY

        c_declarations: "#include <stdio.h>\n#include <stdint.h> \n void print_int(int64_t x){printf(\"%ld\\n\", x);}\n".to_string(),
        c_code: String::new(),

        int_storage,
        functions,
        ident_idx_to_string,
    };
    s.gen_c();

    s.c_declarations.push_str(&s.c_code);
    s.c_declarations
}

impl State<'_> {
    fn spop(&mut self) -> StackItem {
        self.stack.pop().unwrap()
    }
    fn spush(&mut self, e: StackItem) {
        self.stack.push(e);
    }
    fn eat(&mut self) -> Soken {
        self.si.0 = self.si.0.wrapping_add(1);
        self.sokens[self.si.0]
    }
    /// internal logic assertion on nbr of items on stack
    fn sexpect(&mut self, items: usize) {
        if self.stack.len() != items {
            println!();
            println!("got {} stackitems, but wants {}", self.stack.len(), items);
        }
    }
    fn print(&mut self, s: &str) {
        self.c_code.push_str(s);
    }
    fn print_decl(&mut self, s: &str) {
        self.c_declarations.push_str(s);
    }
    fn binop_to_c_name(&self, op: BinaryOp) -> &'static str {
        use BinaryOp as B;
        match op {
            B::SetAdd => "+=",
            B::SetSub => "-=",
            B::Set => "=",
            B::Eql => "==",
            B::Les => "<",
            B::Mor => ">",
            B::Add => "+",
            B::Sub => "-",
            B::Mul => "*",
        }
    }
    /// crazy function, must look at first element of stack, and recursively
    /// do inorder traversal of the ref vec, this can be done in a loop,
    /// but easy recursive version first
    fn print_first_on_stack(&mut self) {
        let e = self.stack.pop().unwrap();
        recurse(self, e);
        use StackItem as SI;
        fn recurse(s: &mut State, e: SI) {
            match e {
                SI::Int(istor) => s.print(&s.int_storage[istor as usize].to_string()),
                SI::Var(ident) => s.print(&s.var_name(ident)),
                SI::Binop(binop, left_idx) => {
                    s.print("(");
                    recurse(s, s.refer_exprs[left_idx as usize]);
                    s.print(s.binop_to_c_name(binop));
                    recurse(s, s.refer_exprs[(left_idx - 1) as usize]);
                    s.print(")");
                }
                SI::FnCall(name, mut fst_arg_idx) => {
                    s.print(&s.func_name(name));
                    s.print("(");
                    let nbr_args = s.functions[&name];
                    for i in 0..nbr_args {
                        if i != 0 {
                            s.print(",")
                        }
                        recurse(s, s.refer_exprs[fst_arg_idx as usize]);
                        fst_arg_idx = fst_arg_idx.wrapping_sub(1);
                    }
                    s.print(")");
                }
            }
        }
    }
    fn var_name(&self, name: IdentIdx) -> String {
        format!("var{}", name)
    }
    fn func_name(&self, name: IdentIdx) -> String {
        let real_name = self.ident_idx_to_string[name as usize];
        match real_name {
            "print_int" => "print_int".to_string(),
            "main" => "main".to_string(),
            _ => format!("func{}", name),
        }
    }

    fn gen_c(&mut self) {
        loop {
            use Soken as S;
            match self.eat() {
                S::EndStat => {
                    self.sexpect(1);
                    self.print_first_on_stack();
                    self.print(";");
                }
                S::Return => {
                    self.sexpect(1);
                    self.print("return ");
                    self.print_first_on_stack();
                    self.print(";");
                }
                // initilize everything to 0
                S::CreateVar(name) => {
                    self.sexpect(0);
                    self.print(&format!("long {}=0;", self.var_name(name)));
                }
                S::FuncDef(name) => {
                    self.sexpect(0);
                    self.print_decl(&format!("long {}(", self.func_name(name)));
                    self.print(&format!("long {}(", self.func_name(name)));
                    let args = *self.functions.get(&name).unwrap();
                    for i in 0..args {
                        if i != 0 {
                            self.print(",");
                            self.print_decl(",");
                        }
                        match self.eat() {
                            Soken::Var(arg) => {
                                self.print_decl(&format!("long {}", self.var_name(arg)));
                                self.print(&format!("long {}", self.var_name(arg)));
                            }
                            _ => unreachable!(),
                        }
                    }
                    self.print_decl(");");
                    self.print("){");
                }
                S::If => {
                    self.sexpect(1);
                    self.print("if (");
                    self.print_first_on_stack();
                    self.print("){");
                }
                S::While => {
                    self.sexpect(1);
                    self.print("while (");
                    self.print_first_on_stack();
                    self.print("){");
                }
                S::ScopeStart => {
                    self.sexpect(0);
                    self.print("{");
                }
                S::ScopeEnd => {
                    self.sexpect(0);
                    self.print("}");
                }
                // HERE BEGINS EXPR SOKENS
                S::Int(intstor) => {
                    self.spush(StackItem::Int(intstor));
                }
                S::Var(e) => {
                    self.spush(StackItem::Var(e));
                }
                // CALL to ALREADY EXISTING function
                S::FuncCall(name, supposed_args) => {
                    for _ in 0..supposed_args {
                        let item = self.spop();
                        self.refer_exprs.push(item);
                    }
                    // take -1 from len to get last one
                    let fst_arg_idx: u16 = self.refer_exprs.len().try_into().unwrap();
                    let fst_arg_idx = fst_arg_idx.overflowing_sub(1).0;
                    self.spush(StackItem::FnCall(name, fst_arg_idx));
                    // function with no args could possibly get u16::MAX, but that's fine because we won't take any args anyway
                }
                S::Binop(binop) => {
                    // put both on refer exprs
                    for _ in 0..2 {
                        let aaa = self.spop();
                        self.refer_exprs.push(aaa);
                    }
                    // on stack, but the binop, looking at top of refer_exprs
                    let fst_arg_idx: u16 = self.refer_exprs.len().try_into().unwrap();
                    let fst_arg_idx = fst_arg_idx.overflowing_sub(1).0;
                    self.spush(StackItem::Binop(binop, fst_arg_idx));
                }
                S::Nil => {
                    unreachable!("ast won't contain nil as they were filtered after ast_verify")
                }
            }
            // this is WEIRD, but bc we start at usize::MAX for `si` we have to do this at end
            if self.si.0 + 1 >= self.sokens.len() {
                break;
            }
        }
    }
}
