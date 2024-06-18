use crate::lexer::IntStor;
use crate::parser::Soken;
use crate::{parser::BinaryOp, IdentIdx};
use std::collections::HashMap;

#[derive(Copy, Clone, Debug)]
struct SokIdx(usize);
struct State<'b> {
    sokens: &'b [Soken],
    si: SokIdx,
    // C code stored hierarchically
    stack: Vec<String>,

    c_declarations: String,
    // constant
    int_stor: &'b IntStor,
    functions: &'b HashMap<IdentIdx, u16>,
    ident_idx_to_string: &'b [&'static str],
}
fn binop_to_c_name(op: BinaryOp) -> &'static str {
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
        int_stor,
        functions,
        ident_idx_to_string,
    };
    s.stack.push(String::from(""));
    s.gen_c();
    let c_code = s.stack.pop().unwrap();
    assert_eq!(s.stack.len(), 0);

    s.c_declarations.push_str(&c_code);
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
    fn print(&mut self, s: &str) {
        self.stack.last_mut().unwrap().push_str(s);
    }
    fn print_decl(&mut self, s: &str) {
        self.c_declarations.push_str(s);
    }
    fn pr_both(&mut self, s: &str) {
        self.print(s);
        self.print_decl(s);
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
        let e = self.spop();
        self.print(&e);
    }
    fn gen_c(&mut self) {
        loop {
            self.gen_c2();
            // this is WEIRD, but bc we start at usize::MAX for `si` we have to do this at end
            if self.si.0 + 1 >= self.sokens.len() {
                break;
            }
        }
    }
    fn gen_c2(&mut self) {
        use Soken as S;
        match self.eat() {
            S::EndStat => {
                let expr = self.spop();
                self.print(&format!("{};", expr));
            }
            S::Return => {
                let expr = self.spop();
                self.print(&format!("return {};", expr));
            }
            // initilize everything to 0
            S::CreateVar(name) => {
                self.print(&format!("int64_t {}=0;", self.var_name(name)));
            }
            S::FuncDef(name) => {
                self.spush(String::new());
                self.pr_both(&format!("int64_t {}(", self.func_name(name)));
                let args = *self.functions.get(&name).unwrap();
                for i in 0..args {
                    if i != 0 {
                        self.pr_both(",");
                    }
                    match self.eat() {
                        Soken::Var(arg) => self.pr_both(&format!("int64_t {}", self.var_name(arg))),
                        _ => unreachable!(),
                    }
                }
                self.print_decl(");");
                self.print("){");
            }
            //  } already printed
            S::EndFuncDef => self.print_first_on_stack(),
            S::If => {
                let (else_part, then_part, cond_part) = (self.spop(), self.spop(), self.spop());
                self.print(&format!("if({cond_part}){then_part}else{else_part}"));
            }
            S::While => {
                let (scope_part, cond_part) = (self.spop(), self.spop());
                self.print(&format!("while({cond_part}){scope_part}"));
            }
            S::StartScope => self.spush(String::from("{")),
            S::EndScope => self.print("}"),
            S::DropScope => self.print_first_on_stack(),

            // HERE BEGINS EXPR SOKENS
            S::Int(intstor) => self.spush(self.int_stor.get(intstor).to_string()),
            S::Var(e) => self.spush(self.var_name(e)),
            // CALL to ALREADY EXISTING function
            S::FuncCall(name, nbr_args) => {
                let mut args = Vec::new();
                for _ in 0..nbr_args {
                    args.push(self.spop());
                }
                self.spush(self.func_name(name));
                self.print("(");
                for (i, arg) in args.into_iter().enumerate() {
                    self.print(&arg);
                    if i as u16 == nbr_args - 1 {
                        break;
                    }
                    self.print(",");
                }
                self.print(")");
            }
            S::Binop(binop) => {
                let (right, left) = (self.spop(), self.spop());
                self.spush(format!("({left}{}{right})", binop_to_c_name(binop)));
            }
            S::Nil => {
                unreachable!("ast won't contain nil as they were filtered after ast_verify")
            }
        }
    }
}
