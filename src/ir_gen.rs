use std::collections::HashMap;

use crate::lexer::IntIdx;
use crate::parser::Soken;
use crate::IdentIdx;

use crate::ir::{
    mk_call, mk_end_func, mk_func_def, mk_jump, mk_jump_req_zero, mk_label, mk_load_int,
    mk_load_reg, mk_op, mk_return, ByteCode, FuncIdx, IRFunc, Label, Op, Reg,
};

#[derive(Copy, Clone, Debug)]
struct SokIdx(usize);

struct Scope {
    instr: Vec<ByteCode>,
    expr_instr: Vec<ByteCode>,
}

struct State<'a> {
    sokens: &'a [Soken],
    si: SokIdx,

    /// scopes like blocks {}, even for if blocks, this is a stack
    /// each scope holds two Vecs of instrs, exprs instrs will be appended onto the real instrs though
    scopes: Vec<Scope>,
    /// holds expr values,
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
    assert_eq!(0, s.scopes.len());
    (final_scope.instr, s.func_array, s.ident_to_func_idx)
}

impl State<'_> {
    fn eat(&mut self) -> Soken {
        let s = self.sokens[self.si.0];
        self.si.0 = self.si.0.wrapping_add(1);
        s
    }
    fn extend_instr(&mut self, instr: &[ByteCode]) {
        self.scopes.last_mut().unwrap().instr.extend(instr);
    }
    /// get's item of of stack, also pushes the expr_instr to the real instructions
    fn pop_expr(&mut self) -> Reg {
        let our_scope = self.scopes.last_mut().unwrap();
        our_scope.instr.extend(&our_scope.expr_instr);
        our_scope.expr_instr.clear();
        self.stack.pop().unwrap()
    }
    /// most outer expr_instr
    fn e_i(&mut self) -> &mut Vec<ByteCode> {
        &mut self.scopes.last_mut().unwrap().expr_instr
    }
    /// most outer statement instr
    fn s_i(&mut self) -> &mut Vec<ByteCode> {
        &mut self.scopes.last_mut().unwrap().instr
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
    /// first implementation is dumb and uses same 'register' representation for 'constants' and variable values
    fn gen_ir2(&mut self) {
        match self.eat() {
            Soken::EndStat => {
                // TODO: 'free' register?? (though not if variable...)
                self.pop_expr();
            }
            Soken::Return => {
                let reg = self.pop_expr();
                mk_return(self.s_i(), reg);
            }
            Soken::Int(intidx) => {
                // mov int into register
                let reg = self.get_reg();
                mk_load_int(self.e_i(), reg, intidx);
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
                mk_load_int(self.e_i(), reg, IntIdx::new(0));
            }
            Soken::Binop(binop) => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                use crate::parser::BinaryOp as B;
                match binop {
                    // left HAS to be variable, so can assume this works
                    B::SetAdd | B::SetSub | B::Set => {
                        match binop {
                            B::SetAdd => mk_op(self.e_i(), left, Op::Add, left, right),
                            B::SetSub => mk_op(self.e_i(), left, Op::Sub, left, right),
                            B::Set => mk_load_reg(self.e_i(), left, right),
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
                        mk_op(self.e_i(), reg, op, left, right);
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
                mk_func_def(self.s_i(), new_name);
            }
            Soken::EndFuncDef(name) => {
                let func = self.scopes.pop().unwrap();
                let our_scope = self.scopes.last_mut().unwrap();
                our_scope.instr.extend(func.instr);
                // FREE ALL THE REGISTERS, because not used between functions
                let func_idx = self.ident_to_func_idx[&name];
                self.func_array[func_idx.0 as usize].regs_used = self.free_reg;
                self.free_reg = 0;
                mk_end_func(self.s_i());
            }
            Soken::FuncCall(name, nr_args) => {
                println!("at func call to {} with nr_args: {}", name, nr_args);
                let mut args = Vec::new();
                for _ in 0..nr_args {
                    let arg_reg = self.stack.pop().unwrap();
                    args.push(arg_reg);
                    println!("arg_reg: {}", arg_reg);
                }
                args.reverse(); // bc get of stack means they are: 3 2 1 0

                let into_reg = self.get_reg();
                let new_name = self.ident_to_func_idx[&name];
                mk_call(self.e_i(), into_reg, new_name, &args);
                // reg will be one over, so can use that as the return place
                self.stack.push(into_reg);
            }
            Soken::If => {
                let else_scope = self.scopes.pop().unwrap();
                let then_scope = self.scopes.pop().unwrap();

                let else_l = self.get_label();
                let end_l = self.get_label();

                let cond_reg = self.pop_expr();
                // generate jump instruction if we got false
                mk_jump_req_zero(self.s_i(), cond_reg, else_l);
                self.extend_instr(&then_scope.instr);
                mk_jump(self.s_i(), end_l);
                mk_label(self.s_i(), else_l);
                self.extend_instr(&else_scope.instr);
                mk_label(self.s_i(), end_l);
            }
            Soken::While => {
                let while_scope = self.scopes.pop().unwrap();

                let before = self.get_label();
                let after = self.get_label();

                mk_label(self.s_i(), before);
                let cond_reg = self.pop_expr();
                mk_jump_req_zero(self.s_i(), cond_reg, after);

                self.extend_instr(&while_scope.instr);
                mk_jump(self.s_i(), before);
                mk_label(self.s_i(), after);
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
