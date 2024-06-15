use crate::parser::Soken;
use std::collections::HashMap;

use crate::{parser::BinaryOp, IdentIdx};

use crate::lexer::IntStor;

#[derive(Copy, Clone, Debug)]
struct SokIdx(usize);
struct State<'b> {
    sokens: &'b [Soken],
    si: SokIdx,
    stack: Vec<String>,

    c_declarations: String,
    c_code: String,
    // constant
    int_stor: &'b IntStor,
    functions: &'b HashMap<IdentIdx, u16>,
    ident_idx_to_string: &'b [&'static str],
}

// check correctness of program + constant folding
pub fn to_c_code(
    sokens: &[Soken],
    ident_idx_to_string: &[&'static str],
    functions: &HashMap<IdentIdx, u16>,
    int_stor: &IntStor,
) -> String {
    let mut s = State {
        sokens,
        si: SokIdx(usize::MAX), // so it wraps to 0 at start, YES HANDLED CORRECTLY
        stack: Vec::new(),

        c_declarations: "#include <stdio.h>\n#include <stdint.h> \n void print_int(int64_t x){printf(\"%ld\\n\", x);}\n".to_string(),
        c_code: String::new(),
        int_stor,
        functions,
        ident_idx_to_string,
    };
    s.gen_c();

    s.c_declarations.push_str(&s.c_code);
    s.c_declarations
}

impl State<'_> {
    fn eat(&mut self) -> Soken {
        self.si.0 = self.si.0.wrapping_add(1);
        self.sokens[self.si.0]
    }
    fn spop(&mut self) -> String {
        self.stack.pop().unwrap()
    }
    fn spush(&mut self, e: String) {
        self.stack.push(e);
    }
    /// internal logic assertion on nbr of items on stack
    fn sexpect(&mut self, items: usize) {
        if self.stack.len() != items {
            println!();
            panic!(
                "C BACKEND ERROR: got {} stackitems, but wants {}",
                self.stack.len(),
                items
            );
        }
    }
    fn print(&mut self, s: &str) {
        self.c_code.push_str(s);
    }
    fn print_decl(&mut self, s: &str) {
        self.c_declarations.push_str(s);
    }
    fn pr_both(&mut self, s: &str) {
        self.print(s);
        self.print_decl(s);
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
    fn print_first_on_stack(&mut self) {
        self.sexpect(1);
        let e = self.stack.pop().unwrap();
        self.print(&e);
    }
    fn gen_c(&mut self) {
        loop {
            use Soken as S;
            match self.eat() {
                S::EndStat => {
                    self.print_first_on_stack();
                    self.print(";");
                }
                S::Return => {
                    self.print("return ");
                    self.print_first_on_stack();
                    self.print(";");
                }
                // initilize everything to 0
                S::CreateVar(name) => {
                    self.sexpect(0);
                    self.print(&format!("int64_t {}=0;", self.var_name(name)));
                }
                S::FuncDef(name) => {
                    self.sexpect(0);
                    self.pr_both(&format!("int64_t {}(", self.func_name(name)));
                    let args = *self.functions.get(&name).unwrap();
                    for i in 0..args {
                        if i != 0 {
                            self.pr_both(",");
                        }
                        match self.eat() {
                            Soken::Var(arg) => {
                                self.pr_both(&format!("int64_t {}", self.var_name(arg)))
                            }
                            _ => unreachable!(),
                        }
                    }
                    self.print_decl(");");
                    self.print("){");
                }
                S::If => {
                    self.print("if(");
                    self.print_first_on_stack();
                    self.print("){");
                }
                S::While => {
                    self.print("while(");
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
                S::Int(intstor) => self.spush(self.int_stor.get(intstor).to_string()),
                S::Var(e) => self.spush(self.var_name(e)),

                // CALL to ALREADY EXISTING function
                S::FuncCall(name, supposed_args) => {
                    let mut res = String::new();
                    res.push_str(&self.func_name(name));
                    res.push('(');
                    let mut args = Vec::new();
                    for _ in 0..supposed_args {
                        args.push(self.spop());
                    }
                    for (i, arg) in args.into_iter().enumerate() {
                        res.push_str(&arg);
                        if i as u16 == supposed_args - 1 {
                            break;
                        }
                        res.push(',');
                    }
                    res.push(')');
                    self.spush(res);
                }
                S::Binop(binop) => {
                    let right = self.spop();
                    let left = self.spop();
                    let mut res = String::from("(");
                    res.push_str(&left);
                    res.push_str(self.binop_to_c_name(binop));
                    res.push_str(&right);
                    res.push(')');
                    self.spush(res);
                }
                S::Nil => {
                    unreachable!("ast won't contain nil as they were filtered after ast_verif")
                }
            }
            // this is WEIRD, but bc we start at usize::MAX for `si` we have to do this at end
            if self.si.0 + 1 >= self.sokens.len() {
                break;
            }
        }
    }
}
