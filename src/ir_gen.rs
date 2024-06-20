use std::collections::HashMap;

use crate::lexer::IntIdx;
use crate::parser::Soken;
use crate::IdentIdx;

use crate::ir::{Reg, FuncIdx, ByteCode, IRFunc, Op, IRHead, Label,};

#[derive(Copy, Clone, Debug)]
struct SokIdx(usize);

struct Scope {
    instr: Vec<ByteCode>,
    expr_instr: Vec<ByteCode>,
}

struct State<'a> {
    sokens: &'a [Soken],
    si: SokIdx,

    scopes: Vec<Scope>,
    stack: Vec<Reg>,

    /// you have a variable, what register was allocated to it?
    varname_to_reg: HashMap<IdentIdx, Reg>,

    /// when looking at function in IR, get it's deadname or args
    func_array: Vec<IRFunc>,
    /// when find funccall in Sokens, get it's func_array idx
    ident_to_func_idx: HashMap<IdentIdx, FuncIdx>,

    /// which register to use next? I need one
    free_reg: Reg,
    /// which label to use next
    free_label: Label,

    functions: &'a HashMap<IdentIdx, u16>,
}

pub fn get_ir(
    sokens: &[Soken],
    functions: &HashMap<IdentIdx, u16>,
) -> (Vec<ByteCode>, Vec<IRFunc>, HashMap<IdentIdx, FuncIdx>) {
    let mut s = State {
        sokens,
        si: SokIdx(0),

        scopes: Vec::new(),
        stack: Vec::new(),

        varname_to_reg: HashMap::new(),
        func_array: Vec::new(),
        ident_to_func_idx: HashMap::new(),

        free_reg: 0,
        free_label: Label(0),

        functions,
    };
    s.scopes.push(Scope {
        instr: Vec::new(),
        expr_instr: Vec::new(),
    });
    for (&name, &args) in functions.iter() {
        let func_label = FuncIdx(s.func_array.len().try_into().unwrap());
        s.func_array.push(IRFunc {
            params: args,
            deadname: name,
            regs_used: 0,
        });
        s.ident_to_func_idx.insert(name, func_label);
    }
    s.gen_ir();
    let final_scope = s.scopes.pop().unwrap();
    // {
    //     for i in 0..1000 {
    //         let mut output = Vec::new();
    //         instr_to_bytecode(final_scope.instr[i], &mut output);
    //     }
    // }
    assert_eq!(0, s.scopes.len());
    (final_scope.instr, s.func_array, s.ident_to_func_idx)
}

impl State<'_> {
    fn eat(&mut self) -> Soken {
        let s = self.sokens[self.si.0];
        self.si.0 = self.si.0.wrapping_add(1);
        s
    }
    fn push(&mut self, instr: ByteCode) {
        self.scopes.last_mut().unwrap().instr.push(instr);
    }
    fn extend_instr(&mut self, instr: &[ByteCode]) {
        self.scopes.last_mut().unwrap().instr.extend(instr);
    }
    /// push instruction that is part of temporary expression
    fn push_e(&mut self, instr: ByteCode) {
        self.scopes.last_mut().unwrap().expr_instr.push(instr);
    }
    /// get's item of of stack, also pushes the expr_instr to the real instructions
    fn pop_expr(&mut self) -> Reg {
        let our_scope = self.scopes.last_mut().unwrap();
        our_scope.instr.extend(&our_scope.expr_instr);
        our_scope.expr_instr.clear();
        self.stack.pop().unwrap()
    }

    /// lots of functions here for generating IR
    fn mk_load_reg(&mut self, r1: Reg, r2: Reg) {
        self.push_e(IRHead::LoadReg as _);
        self.push_e(r1 as _);
        self.push_e(r2 as _);
    }
    fn mk_load_int(&mut self, r1: Reg, intidx: IntIdx) {
        self.push_e(IRHead::LoadInt as _);
        self.push_e(r1 as _);
        self.push_e(unsafe{std::mem::transmute(intidx)});
    }
    fn mk_op(&mut self, r1: Reg, op: Op, r2: Reg, r3: Reg) {
        self.push_e(IRHead::Op as _);
        self.push_e(r1 as _);
        self.push_e(op as _);
        self.push_e(r2 as _);
        self.push_e(r3 as _);
    }
    fn mk_call(&mut self, fnidx: FuncIdx, r1: Reg, r2: Reg) {
        self.push_e(IRHead::Call as _);
        self.push_e(fnidx.0);
        self.push_e(r1 as _);
        self.push_e(r2 as _);
    }
    fn mk_jump(&mut self, label: Label) {
        self.push(IRHead::Jump as _);
        self.push(label.0);
    }
    fn mk_jump_req_zero(&mut self, r1: Reg, label: Label) {
        self.push(IRHead::JumpRegZero as _);
        self.push(r1 as _);
        self.push(label.0);
    }
    fn mk_label(&mut self, label: Label) {
        self.push(IRHead::Label as _);
        self.push(label.0);
    }
    fn mk_return(&mut self, r1: Reg) {
        self.push(IRHead::Return as _);
        self.push(r1 as _);
    }
    fn mk_func_def(&mut self, fnidx: FuncIdx) {
        println!("make FUNCDEF: {:?}", fnidx);
        self.push(IRHead::FuncDef as _);
        self.push(fnidx.0);
    }
    fn mk_end_func(&mut self) {
        self.push(IRHead::EndFunc as _);
    }

    fn get_reg(&mut self) -> Reg {
        let ret = self.free_reg;
        self.free_reg += 1;
        ret
    }
    fn get_label(&mut self) -> Label {
        let ret = self.free_label;
        self.free_label.0 += 1;
        ret
    }
    fn gen_ir(&mut self) {
        loop {
            self.gen_ir2();
            if self.si.0 >= self.sokens.len() {
                break;
            }
        }
    }
    /// first implementation is dumb and uses same 'register' representationfor 'constants' and variable values
    fn gen_ir2(&mut self) {
        match self.eat() {
            Soken::EndStat => {
                // TODO: 'free' register?? (though not if variable...)
                self.pop_expr();
            }
            Soken::Return => {
                let reg = self.pop_expr();
                self.mk_return(reg);
            }
            Soken::Int(intidx) => {
                // mov int into register
                let reg = self.get_reg();
                self.mk_load_int(reg, intidx);
                self.stack.push(reg);
            }
            Soken::Var(varidx) => {
                // maybe we can just push to the stack the reg with the variable?
                let reg = self.varname_to_reg[&varidx];
                self.stack.push(reg)
            }
            Soken::CreateVar(varidx) => {
                // set a new free register to 0
                let reg = self.get_reg();
                self.varname_to_reg.insert(varidx, reg);
                self.mk_load_int(reg, IntIdx::new(0));
            }
            Soken::Binop(binop) => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                use crate::parser::BinaryOp as B;
                match binop {
                    // left HAS to be variable, so can assume this works
                    B::SetAdd | B::SetSub | B::Set => {
                        match binop {
                            B::SetAdd => self.mk_op(left, Op::Add, left, right),
                            B::SetSub => self.mk_op(left, Op::Sub, left, right),
                            B::Set => self.mk_load_reg(left, right),
                            _ => unreachable!(),
                        };
                        self.stack.push(left);
                    }
                    // both could be variables, so don't overwrite any of them
                    // create new register
                    B::Eql | B::Les | B::Mor | B::Add | B::Sub | B::Mul => {
                        let reg = self.get_reg();
                        let op = match binop {
                            B::Add => Op::Add,
                            B::Sub => Op::Sub,
                            B::Mul => Op::Mul,
                            B::Eql => Op::Eql,
                            B::Les => Op::Les,
                            B::Mor => Op::Mor,
                            _ => unreachable!(),
                        };
                        self.mk_op(reg, op, left, right);
                        self.stack.push(reg);
                    }
                }
            }
            Soken::FuncDef(name) => {
                let args = *self.functions.get(&name).unwrap();
                // use up X amount of registers
                for _ in 0..args {
                    let reg = self.get_reg();
                    let arg = self.eat();
                    if let Soken::Var(ident) = arg {
                        self.varname_to_reg.insert(ident, reg);
                    } else {
                        unreachable!()
                    }
                }
                self.scopes.push(Scope {
                    instr: Vec::new(),
                    expr_instr: Vec::new(),
                });
                let new_name = self.ident_to_func_idx[&name];
                self.mk_func_def(new_name);
            }
            Soken::EndFuncDef(name) => {
                let func = self.scopes.pop().unwrap();
                let our_scope = self.scopes.last_mut().unwrap();
                our_scope.instr.extend(func.instr);
                // FREE ALL THE REGISTERS, because not used between functions
                let func_idx = self.ident_to_func_idx[&name];
                self.func_array[func_idx.0 as usize].regs_used = self.free_reg;
                self.free_reg = 0;
                self.mk_end_func();
            }
            Soken::FuncCall(name, nr_args) => {
                // TODO get regs of the arguments and then assign to NEW regs, which will be range which function will be called with.
                println!("at func call to {} with nr_args: {}", name, nr_args);
                let mut args = Vec::new();
                for _ in 0..nr_args {
                    let arg_reg = self.stack.pop().unwrap();
                    args.push(arg_reg);
                    println!("arg_reg: {}", arg_reg);
                }
                let first_reg = self.get_reg();
                let mut reg = first_reg;
                while let Some(arg) = args.pop() {
                    self.mk_load_reg(reg, arg);
                    reg = self.get_reg();
                }
                // reg will be one over, so can use that as the return place
                let new_name = self.ident_to_func_idx[&name];
                self.mk_call(new_name, first_reg, reg);
                self.stack.push(reg);
            }
            Soken::If => {
                let else_scope = self.scopes.pop().unwrap();
                let then_scope = self.scopes.pop().unwrap();

                let else_l = self.get_label();
                let end_l = self.get_label();

                let cond_reg = self.pop_expr();
                // generate jump instruction if we got false
                self.mk_jump_req_zero(cond_reg, else_l);
                self.extend_instr(&then_scope.instr);
                self.mk_jump(end_l);
                self.mk_label(else_l);
                self.extend_instr(&else_scope.instr);
                self.mk_label(end_l);
            }
            Soken::While => {
                let while_scope = self.scopes.pop().unwrap();

                let before = self.get_label();
                let after = self.get_label();

                self.mk_label(before);
                let cond_reg = self.pop_expr();
                self.mk_jump_req_zero(cond_reg, after);

                self.extend_instr(&while_scope.instr);
                self.mk_jump(before);
                self.mk_label(after);
            }
            Soken::StartScope => {
                // TODO: Why do I create a new scope here, is it really necessary?
                // Well yes, because If/While has to get the scope later
                self.scopes.push(Scope {
                    instr: Vec::new(),
                    expr_instr: Vec::new(),
                });
            }
            // TODO: free registers?
            Soken::EndScope => (),
            // TODO: free registers?
            Soken::DropScope => {
                self.scopes.pop().unwrap();
            }
            Soken::Nil => unreachable!("no nils"),
        }
    }
}
