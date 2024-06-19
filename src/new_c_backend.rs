use std::collections::HashMap;

use crate::{
    ir_gen::{FuncIdx, IRFunc, Instr, Label, Reg},
    lexer::IntStor,
    IdentIdx,
};

/// register name
fn r_n(r: Reg) -> String {
    format!("r{}", r)
}
/// label name
fn l_n(l: Label) -> String {
    format!("label{}", l.0)
}

pub fn gen_c(
    ir: &[Instr],
    ir_functions: &[IRFunc],
    intstor: &IntStor,
    ident_to_func_idx: HashMap<IdentIdx, FuncIdx>,
    print_int_ident_idx: IdentIdx,
    ident_idx_to_string: &[&'static str],
) -> String {
    let mut c_declarations = String::from("#include <stdio.h>\n#include <stdint.h>\n\n");
    macro_rules! f_n {
        ($f:expr) => {{
            let irfunc = &ir_functions[$f.0 as usize];
            let orig_name = ident_idx_to_string[irfunc.deadname as usize];
            if orig_name == "main" {
                String::from("main")
            } else {
                format!("func{}", $f.0)
            }
        }};
    }
    c_declarations.push_str(&format!(
        "int64_t {}(int64_t x){{printf(\"%ld\\n\",x);}}\n",
        f_n!(ident_to_func_idx[&print_int_ident_idx])
    ));

    let mut c_code = String::new();
    for &instr in ir {
        match instr {
            Instr::LoadReg(to, from) => c_code.push_str(&format!("{}={};", r_n(to), r_n(from))),
            Instr::LoadInt(to, intidx) => {
                let the_int = intstor.get(intidx);
                c_code.push_str(&format!("{}={};", r_n(to), the_int))
            }
            Instr::Op(to, op, left, right) => {
                use crate::ir_gen::Op as O;
                let c_op = match op {
                    O::Add => "+",
                    O::Sub => "-",
                    O::Mul => "*",
                    O::Mor => ">",
                    O::Les => "<",
                    O::Eql => "==",
                };
                c_code.push_str(&format!("{}={}{}{};", r_n(to), r_n(left), c_op, r_n(right)));
            }
            Instr::Jump(label) => c_code.push_str(&format!("goto {};", l_n(label))),

            Instr::JumpRegZero(reg, label) => {
                c_code.push_str(&format!("if (0=={}) goto {};", r_n(reg), l_n(label)))
            }
            Instr::Label(label) => c_code.push_str(&format!("{}:", l_n(label))),
            Instr::Return(reg) => c_code.push_str(&format!("return {};", r_n(reg))),
            Instr::FuncDef(fnidx) => {
                let func = &ir_functions[fnidx.0 as usize];
                c_code.push_str(&format!("int64_t {}(", f_n!(fnidx)));
                c_declarations.push_str(&format!("int64_t {}(", f_n!(fnidx)));
                for i in 0..func.params {
                    c_code.push_str(&format!("int64_t {}", r_n(i)));
                    c_declarations.push_str(&format!("int64_t {}", r_n(i)));
                    if i == func.params - 1 {
                        break;
                    }
                    c_code.push(',');
                    c_declarations.push(',');
                }
                c_code.push_str("){");
                c_declarations.push_str(");");
                if func.regs_used - func.params > 0 {
                    c_code.push_str("int64_t ");
                    for i in func.params..func.regs_used {
                        if i != func.params {
                            c_code.push(',');
                        }
                        c_code.push_str(&r_n(i));
                    }
                    c_code.push_str(";");
                }
            }
            Instr::EndFunc => c_code.push('}'),
            Instr::Call(fnidx, firstreg, to) => {
                c_code.push_str(&format!("{}={}(", r_n(to), f_n!(fnidx)));
                let func = &ir_functions[fnidx.0 as usize];
                for i in 0..func.params {
                    c_code.push_str(&r_n(firstreg + i));
                    if i == func.params - 1 {
                        break;
                    }
                    c_code.push(',');
                }
                c_code.push_str(");");
            }
        }
    }
    c_declarations.push_str(&c_code);
    c_declarations
}
