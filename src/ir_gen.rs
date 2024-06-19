use std::collections::HashMap;

use crate::lexer::IntIdx;
use crate::parser::Soken;
use crate::IdentIdx;

pub type Reg = u16;

#[derive(Clone, Copy, Debug)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Mor,
    Les,
    Eql,
}

/// in instructions we will have labels
/// could possibly have hashmap saying where on is
/// can jump to them
#[derive(Clone, Copy, Debug)]
pub struct Label(pub u16);

/// index into func_array
#[derive(Clone, Copy, Debug)]
pub struct FuncIdx(pub u16);

/// data for a function in IR, gotten by indexing into func_array array
pub struct IRFunc {
    pub params: u16,
    pub deadname: IdentIdx,
    pub regs_used: u16,
}

/// turn into 2-byte bytecode
#[derive(Clone, Copy, Debug)]
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
    /// start of function, could do some stuff with this
    FuncDef(FuncIdx),
    /// signals the end of function definition, useful in c_backend
    EndFunc,
    /// call .0 with register starting at .1 (look up how many args) put result into .2
    Call(FuncIdx, Reg, Reg),
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
) -> (Vec<Instr>, Vec<IRFunc>, HashMap<IdentIdx, FuncIdx>) {
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
        instrs: Vec::new(),
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
    assert_eq!(0, s.scopes.len());
    (final_scope.instrs, s.func_array, s.ident_to_func_idx)
}

impl State<'_> {
    fn eat(&mut self) -> Soken {
        let s = self.sokens[self.si.0];
        self.si.0 = self.si.0.wrapping_add(1);
        s
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
    fn gen_ir(&mut self) {
        loop {
            self.gen_ir2();
            if self.si.0 >= self.sokens.len() {
                break;
            }
        }
    }
    /// get's item of of stack, also pushes the expr_instrs to the real instructions
    fn pop_expr(&mut self) -> Reg {
        let our_scope = self.scopes.last_mut().unwrap();
        our_scope.instrs.extend(&our_scope.expr_instr);
        our_scope.expr_instr.clear();
        self.stack.pop().unwrap()
    }
    /// first implementation is dumb and uses same registers for 'constants' and
    /// variable values
    fn gen_ir2(&mut self) {
        match self.eat() {
            Soken::EndStat => {
                // TODO: 'free' register?? (though not if variable...)
                self.pop_expr();
            }
            Soken::Return => {
                let reg = self.pop_expr();
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
                            B::Eql => Op::Eql,
                            B::Les => Op::Les,
                            B::Mor => Op::Mor,
                            _ => unreachable!(),
                        };
                        self.push_e_instr(Instr::Op(reg, op, left, right));
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
                    instrs: Vec::new(),
                    expr_instr: Vec::new(),
                });
                let new_name = self.ident_to_func_idx[&name];
                self.push_instr(Instr::FuncDef(new_name));
            }
            Soken::EndFuncDef(name) => {
                let func = self.scopes.pop().unwrap();
                let our_scope = self.scopes.last_mut().unwrap();
                our_scope.instrs.extend(func.instrs);
                // FREE ALL THE REGISTERS, because not used between functions
                let func_idx = self.ident_to_func_idx[&name];
                self.func_array[func_idx.0 as usize].regs_used = self.free_reg;
                self.free_reg = 0;
                our_scope.instrs.push(Instr::EndFunc);
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
                    self.push_e_instr(Instr::LoadReg(reg, arg));
                    reg = self.get_reg();
                }
                // reg will be one over, so can use that as the return place
                let new_name = self.ident_to_func_idx[&name];
                self.push_e_instr(Instr::Call(new_name, first_reg, reg));
                self.stack.push(reg);
            }
            Soken::If => {
                let else_scope = self.scopes.pop().unwrap();
                let then_scope = self.scopes.pop().unwrap();

                let else_l = self.get_label();
                let end_l = self.get_label();

                let cond_reg = self.pop_expr();
                let our_scope = self.scopes.last_mut().unwrap();
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

                let before = self.get_label();
                let after = self.get_label();

                let our_scope = self.scopes.last_mut().unwrap();
                our_scope.instrs.push(Instr::Label(before));
                let cond_reg = self.pop_expr();
                let our_scope = self.scopes.last_mut().unwrap();
                our_scope.instrs.push(Instr::JumpRegZero(cond_reg, after));

                our_scope.instrs.extend(&while_scope.instrs);
                our_scope.instrs.push(Instr::Jump(before));
                our_scope.instrs.push(Instr::Label(after));
            }
            Soken::StartScope => {
                // TODO: Why do I create a new scope here, is it really necessary?
                // Well yes, because If/While has to get the scope later
                self.scopes.push(Scope {
                    instrs: Vec::new(),
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
