use crate::{
    ir::{FuncIdx, IRFunc, Instr, InstrIterator, Op, Reg},
    lexer::IntStor,
    IdentIdx,
};

pub fn interpret(
    ir: &mut InstrIterator,
    ir_functions: &[IRFunc],
    intstor: &IntStor,
    print_int_ident_idx: IdentIdx,
    ident_idx_to_string: &[&'static str],
) {
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

    /// EXPLANATION
    /// We have a fixed buffer called reg_stack. When we call a function
    /// we increase the stack pointer with how many regs it needs, then
    /// when doing stuff with the function, it will negatively index from the end
    /// of the stack, to get it's regs.
    /// We also have a Frame Vec, which holds data like, where to return to
    /// how many regs should we deallocate when returning, what register in the
    /// previous stack frame should we set?
    struct Frame {
        called_from: usize,
        reg_to_set: Reg,
        regs_used: usize,
    }
    let mut frames = vec![Frame {
        called_from: usize::MAX,
        reg_to_set: 0,
        regs_used: ir_functions[main_func.0 as usize].regs_used as usize,
    }];
    /// get the latest stack frame, also a pun on the swedish word 'fram' which means front
    macro_rules! fram {
        () => {{
            frames.last_mut().unwrap()
        }};
    }
    // where registers go
    let mut reg_stack = vec![0; 1 << 12];
    let mut stack_pointer = fram!().regs_used;

    // start at main
    ir.at = funcs_at[main_func.0 as usize];

    // index your current stack frame for regs
    macro_rules! get_reg {
        [$e:expr] => {
            reg_stack[stack_pointer.checked_add_signed(-($e as isize)).unwrap() ]
        };
    }

    while let Some(instr) = ir.next() {
        // println!("lets interpret: {:?}", instr);
        match instr {
            Instr::LoadReg(r1, r2) => {
                get_reg![r1] = get_reg![r2];
            }
            Instr::LoadInt(reg, intidx) => {
                get_reg![reg] = intstor.get(intidx);
            }
            Instr::Op(to, op, left, right) => {
                let l = get_reg![left];
                let r = get_reg![right];
                let res = match op {
                    Op::Add => l + r,
                    Op::Sub => l - r,
                    Op::Mul => l * r,
                    Op::Mor => (l > r) as i64,
                    Op::Les => (l < r) as i64,
                    Op::Eql => (l == r) as i64,
                };
                get_reg![to] = res;
            }
            Instr::Call(to, fnidx, args) => {
                if ir_functions[fnidx.0 as usize].deadname == print_int_ident_idx {
                    println!("interprint: {}", get_reg![args[0]]);
                } else {
                    frames.push(Frame {
                        called_from: 0,
                        reg_to_set: to,
                        regs_used: ir_functions[fnidx.0 as usize].regs_used as usize,
                    });
                    let regs_used = fram!().regs_used;
                    // set first regs to args
                    for i in 0..args.len() {
                        get_reg![i as isize - regs_used as isize] = get_reg![args[i]];
                    }
                    stack_pointer += regs_used;
                    fram!().called_from = ir.at;
                    ir.at = funcs_at[fnidx.0 as usize];
                }
            }
            Instr::Jump(label) => {
                ir.at = labels_at[label.0 as usize];
            }
            Instr::JumpRegZero(reg, label) => {
                if 0 == get_reg![reg] {
                    ir.at = labels_at[label.0 as usize];
                }
            }
            Instr::Return(to_return) => {
                let frame = frames.pop().unwrap();
                if frames.is_empty() {
                    break; // exit interpretation
                }
                let ret_value = get_reg!(to_return);
                stack_pointer -= frame.regs_used;
                get_reg![frame.reg_to_set] = ret_value;
                ir.at = frame.called_from;
            }
            Instr::Label(_) => (),
            Instr::FuncDef(_) => unreachable!(),
            Instr::EndFunc => unreachable!(),
        }
    }
}
