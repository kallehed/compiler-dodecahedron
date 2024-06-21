use std::collections::HashMap;

use crate::{
    ir::{FuncIdx, IRFunc, Instr, InstrIterator, Label, Reg},
    lexer::IntStor,
    IdentIdx,
};

pub fn interpret(
    ir: &mut InstrIterator,
    ir_functions: &[IRFunc],
    intstor: &IntStor,
    ident_to_func_idx: &HashMap<IdentIdx, FuncIdx>,
    print_int_ident_idx: IdentIdx,
    ident_idx_to_string: &[&'static str],
) {
    /// takes in FuncIdent
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

    // get where all the functions START, and where we should start
    let (funcs_at, main_func, labels_at) = {
        let mut funcs_at = vec![0; ir_functions.len()];
        let mut labels_at = vec![];
        let mut main_func = FuncIdx(0);
        while let Some(instr) = ir.next() {
            match instr {
                Instr::FuncDef(fnidx) => {
                    // ir.at should now be at bytecode AFTER FuncDef
                    funcs_at[fnidx.0 as usize] = ir.at;
                    if "main"
                        == ident_idx_to_string[ir_functions[fnidx.0 as usize].deadname as usize]
                    {
                        main_func = fnidx;
                    }
                }
                Instr::Label(label) => {
                    while labels_at.len() <= label.0 as usize {
                        labels_at.push(0);
                    }
                    labels_at[label.0 as usize] = ir.at;
                }
                _ => (),
            }
        }
        (funcs_at, main_func, labels_at)
    };

    println!("the ir functions: {:?}", ir_functions);

    // start at main
    ir.at = funcs_at[main_func.0 as usize];
    struct Frame {
        regs: Vec<i64>,
        called_from: usize,
        reg_to_set: Reg,
    }
    let mut frames = vec![Frame {
        regs: vec![0; ir_functions[main_func.0 as usize].regs_used as usize],
        called_from: usize::MAX,
        reg_to_set: 0,
    }];
    macro_rules! fram {
        () => {{
            frames.last_mut().unwrap()
        }};
    }
    while let Some(instr) = ir.next() {
        // println!("lets interpret: {:?}", instr);
        match instr {
            Instr::LoadReg(r1, r2) => {
                fram!().regs[r1 as usize] = fram!().regs[r2 as usize];
            }
            Instr::LoadInt(reg, intidx) => {
                fram!().regs[reg as usize] = intstor.get(intidx);
            }
            Instr::Op(to, op, left, right) => {
                let l = fram!().regs[left as usize];
                let r = fram!().regs[right as usize];
                let res = match op {
                    crate::ir::Op::Add => l + r,
                    crate::ir::Op::Sub => l - r,
                    crate::ir::Op::Mul => l * r,
                    crate::ir::Op::Mor => (l > r) as i64,
                    crate::ir::Op::Les => (l < r) as i64,
                    crate::ir::Op::Eql => (l == r) as i64,
                };
                fram!().regs[to as usize] = res;
            }
            Instr::Call(to, fnidx, args) => {
                if ir_functions[fnidx.0 as usize].deadname == print_int_ident_idx {
                    println!("interp: {}", fram!().regs[args[0] as usize]);
                } else {
                    frames.push(Frame {
                        regs: vec![0; (ir_functions[fnidx.0 as usize].regs_used) as usize],
                        called_from: 0,
                        reg_to_set: to,
                    });
                    // set first regs to args
                    for i in 0..args.len() {
                        fram!().regs[i] = frames[frames.len() - 2].regs[args[i] as usize];
                    }
                    fram!().called_from = ir.at;
                    ir.at = funcs_at[fnidx.0 as usize];
                }
            }
            Instr::Jump(label) => {
                ir.at = labels_at[label.0 as usize];
            }
            Instr::JumpRegZero(reg, label) => {
                if 0 == fram!().regs[reg as usize] {
                    ir.at = labels_at[label.0 as usize];
                }
            }
            Instr::Return(to_return) => {
                let frame = frames.pop().unwrap();
                if frames.len() == 0 {
                    break; // exit interpretation
                }
                fram!().regs[frame.reg_to_set as usize] = frame.regs[to_return as usize];
                ir.at = frame.called_from;
            }

            Instr::Label(_) => (),
            Instr::FuncDef(_) => unreachable!(),
            Instr::EndFunc => unreachable!(),
        }
    }
}
