use std::collections::HashMap;

use crate::lexer::IntIdx;
use crate::parser::Soken;
use crate::IdentIdx;

type Reg = u16;

#[derive(Clone, Copy)]
pub enum Op {
    Add,
    Sub,
    Mul,
}

/// in instructions we will have labels
/// could possibly have hashmap saying where on is
/// can jump to them
#[derive(Clone, Copy)]
pub struct Label(u16);

#[derive(Clone, Copy)]
pub enum Instr {
    /// load value of one register into another
    LoadReg(Reg, Reg),
    /// load Int into register
    LoadInt(Reg, IntIdx),
    // .0 = .2 `.1` .3
    Op(Reg, Op, Reg, Reg),
    /// jump to label
    Jump(Label),
    /// Jump if register is zero
    JumpRegZero(Reg, Label),
    /// label to which you could jump
    Label(Label),
    /// return from function, return value is reg
    Return(Reg),
}

#[derive(Copy, Clone, Debug)]
struct SokIdx(usize);

struct Scope {
    instrs: Vec<Instr>,
    expr_instr: Vec<Instr>,
}

struct State<'a> {
    sokens: &'a [Soken],
    si: SokIdx,

    scopes: Vec<Scope>,
    stack: Vec<Reg>,

    /// you have a variable, what register was allocated to it?
    varname_to_reg: HashMap<IdentIdx, Reg>,

    /// which register to use next? I need one
    free_reg: Reg,
    /// which label to use next
    free_label: Label,

    functions: &'a HashMap<IdentIdx, u16>,
}

pub fn get_ir(sokens: &[Soken], functions: &HashMap<IdentIdx, u16>) -> Vec<Instr> {
    let mut s = State {
        sokens,
        si: SokIdx(0),

        scopes: Vec::new(),
        stack: Vec::new(),

        varname_to_reg: HashMap::new(),

        free_reg: 0,
        free_label: Label(0),

        functions,
    };
    s.scopes.push(Scope {
        instrs: Vec::new(),
        expr_instr: Vec::new(),
    });
    s.gen_ir();
    let final_scope = s.scopes.pop().unwrap();
    assert_eq!(0, s.scopes.len());
    final_scope.instrs
}

impl State<'_> {
    fn eat(&mut self) -> Soken {
        let s = self.sokens[self.si.0];
        self.si.0 = self.si.0.wrapping_add(1);
        s
    }
    fn gen_ir(&mut self) {
        loop {
            self.gen_ir2();
            if self.si.0 >= self.sokens.len() {
                break;
            }
        }
    }
    fn push_instr(&mut self, instr: Instr) {
        self.scopes.last_mut().unwrap().instrs.push(instr);
    }
    fn push_e_instr(&mut self, instr: Instr) {
        self.scopes.last_mut().unwrap().expr_instr.push(instr);
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
    /// first implementation is dumb and uses same registers for 'constants' and
    /// variable values
    fn gen_ir2(&mut self) {
        match self.eat() {
            Soken::EndStat => {
                // TODO: 'free' register?? (though not if variable...)
                self.stack.pop().unwrap();
                let our_scope = self.scopes.last_mut().unwrap();
                our_scope.instrs.extend(&our_scope.expr_instr);
                our_scope.expr_instr.clear();
            }
            Soken::Return => {
                let reg = self.stack.pop().unwrap();
                self.push_instr(Instr::Return(reg));
            }
            Soken::Int(intidx) => {
                // mov int into register
                let reg = self.get_reg();
                self.push_e_instr(Instr::LoadInt(reg, intidx));
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
                self.push_e_instr(Instr::LoadInt(reg, IntIdx::new(0)));
            }
            Soken::Binop(binop) => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                use crate::parser::BinaryOp as B;
                match binop {
                    // left HAS to be variable, so can assume this works
                    B::SetAdd | B::SetSub | B::Set => {
                        match binop {
                            B::SetAdd => self.push_e_instr(Instr::Op(left, Op::Add, left, right)),
                            B::SetSub => self.push_e_instr(Instr::Op(left, Op::Sub, left, right)),
                            B::Set => self.push_e_instr(Instr::LoadReg(left, right)),
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
                            B::Eql => todo!(),
                            B::Les => todo!(),
                            B::Mor => todo!(),
                            _ => unreachable!(),
                        };
                        self.push_e_instr(Instr::Op(reg, op, left, right));
                        self.stack.push(reg);
                    }
                }
            }
            Soken::FuncDef(name) => {
                let args = *self.functions.get(&name).unwrap();
                for _ in 0..args {
                    self.get_reg();
                    self.eat();
                }
                let func_start_label = self.get_label();
                self.scopes.push(Scope {
                    instrs: Vec::new(),
                    expr_instr: Vec::new(),
                });
                self.push_instr(Instr::Label(func_start_label));
            }
            Soken::EndFuncDef => {
                let func = self.scopes.pop().unwrap();
                let our_scope = self.scopes.last_mut().unwrap();
                our_scope.instrs.extend(func.instrs);
            }
            Soken::FuncCall(name, nr_args) => {
                // TODO get regs of the arguments and then assign to NEW regs, which will be range which function will be called with.
            }
            Soken::If => {
                let else_scope = self.scopes.pop().unwrap();
                let then_scope = self.scopes.pop().unwrap();
                let cond_reg = self.stack.pop().unwrap();

                let else_l = self.get_label();
                let end_l = self.get_label();

                let our_scope = self.scopes.last_mut().unwrap();

                our_scope.instrs.extend(&our_scope.expr_instr);
                our_scope.expr_instr.clear();
                // generate jump instruction if we got false
                our_scope.instrs.push(Instr::JumpRegZero(cond_reg, else_l));
                our_scope.instrs.extend(&then_scope.instrs);
                our_scope.instrs.push(Instr::Jump(end_l));

                our_scope.instrs.push(Instr::Label(else_l));
                our_scope.instrs.extend(&else_scope.instrs);
                our_scope.instrs.push(Instr::Label(end_l));
            }
            Soken::While => {
                let while_scope = self.scopes.pop().unwrap();
                let cond_reg = self.stack.pop().unwrap();

                let before = self.get_label();
                let after = self.get_label();

                let our_scope = self.scopes.last_mut().unwrap();

                our_scope.instrs.push(Instr::Label(before));
                our_scope.instrs.extend(&our_scope.expr_instr);
                our_scope.expr_instr.clear();
                our_scope.instrs.push(Instr::JumpRegZero(cond_reg, after));

                our_scope.instrs.extend(&while_scope.instrs);
                our_scope.instrs.push(Instr::Jump(before));
                our_scope.instrs.push(Instr::Label(after));
            }
            Soken::StartScope => {
                self.scopes.push(Scope {
                    instrs: Vec::new(),
                    expr_instr: Vec::new(),
                });
            }
            Soken::EndScope => (),
            Soken::DropScope => {
                self.scopes.pop().unwrap();
            }
            Soken::Nil => unreachable!("no nils"),
        }
    }
}
