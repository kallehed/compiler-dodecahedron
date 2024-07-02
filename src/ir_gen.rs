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

enum StackItem {
    Unit,
    NotYetPlacedInt(IntIdx),
    S(SomeReg),
}

enum SomeReg {
    Tmp(Reg),
    Var(Reg),
}

struct State<'a> {
    sokens: &'a [Soken],
    si: SokIdx,

    /// scopes like blocks {}, even for if blocks, this is a stack
    /// each scope holds two Vecs of instrs, exprs instrs will be appended onto the real instrs though
    scopes: Vec<Scope>,
    /// holds expr values,
    stack: Vec<StackItem>,

    /// you have a variable, what register was allocated to it?
    varname_to_reg: HashMap<IdentIdx, Reg>,

    /// when looking at function in IR, get it's deadname or args
    func_array: Vec<IRFunc>,
    /// when find funccall in Sokens, get it's func_array idx
    ident_to_func_idx: HashMap<IdentIdx, FuncIdx>,

    /// which register to use next? I need one
    free_reg: Reg,
    // watermark of nbr of regs used in function
    reg_func_watermark: Reg,
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
        reg_func_watermark: 0,
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
        self.si.0 += 1;
        s
    }
    fn extend_instr(&mut self, instr: &[ByteCode]) {
        self.scopes.last_mut().unwrap().instr.extend(instr);
    }
    /// get's item of of stack, also pushes the expr_instr to the real instructions
    fn pop_expr(&mut self) -> StackItem {
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

    /// make stackitem available as some kind of register
    /// this turns a number into something that can be used
    fn realize_stackitem(&mut self, s: StackItem, for_expr: bool) -> SomeReg {
        match s {
            StackItem::NotYetPlacedInt(intidx) => {
                let reg = self.get_reg();
                mk_load_int(if for_expr { self.e_i() } else { self.s_i() }, reg, intidx);
                SomeReg::Tmp(reg)
            }
            StackItem::S(some) => some,
            StackItem::Unit => unreachable!(),
        }
    }

    /// says that register will be used now, but future computations can
    /// reuse IF it was temporary
    fn use_some_reg(&mut self, s: SomeReg) -> Reg {
        match s {
            SomeReg::Tmp(reg) => {
                self.free_reg -= 1;
                reg
            }
            SomeReg::Var(var) => var,
        }
    }

    // allocate new register which is unused
    fn get_reg(&mut self) -> Reg {
        let ret = self.free_reg;
        self.free_reg += 1;
        if self.free_reg > self.reg_func_watermark {
            self.reg_func_watermark = self.free_reg;
        }
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
    /// also puts the expr stuff in stat instr
    fn expr_to_usable_once_reg(&mut self) -> Reg {
        let stackitem = self.pop_expr();
        let some_reg = self.realize_stackitem(stackitem, false);
        self.use_some_reg(some_reg)
    }
    /// first implementation is dumb and uses same 'register' representation for 'constants' and variable values
    fn gen_ir2(&mut self) {
        match self.eat() {
            Soken::EndStat => {
                let our_scope = self.scopes.last_mut().unwrap();
                our_scope.instr.extend(&our_scope.expr_instr);
                our_scope.expr_instr.clear();
                // if expr on stack exists, potentially free it's temp reg
                match self.stack.pop().unwrap() {
                    StackItem::NotYetPlacedInt(_) => (),
                    StackItem::S(s) => match s {
                        SomeReg::Tmp(_) => self.free_reg -= 1,
                        SomeReg::Var(_) => (),
                    },
                    StackItem::Unit => (),
                }
            }
            Soken::Return => {
                let reg = self.expr_to_usable_once_reg();
                mk_return(self.s_i(), reg);
            }
            Soken::Int(intidx) => {
                // don't be eager, could use less registers if we don't place the number into a register yet
                self.stack.push(StackItem::NotYetPlacedInt(intidx))
            }
            //push to the stack the reg with the variable
            Soken::RVar(varidx) => {
                let reg = self.varname_to_reg[&varidx];
                self.stack.push(StackItem::S(SomeReg::Var(reg)))
            }
            // same, TODO: remove Lvalues or whatever
            Soken::LVar(varidx) => {
                let reg = self.varname_to_reg[&varidx];
                self.stack.push(StackItem::S(SomeReg::Var(reg)))
            }
            // we don't really care if your variable is mutable or not, because it's still just in memory, and we know the program is correct, so no checking is needed
            Soken::InitVar(varidx, _mutable) => {
                // get new reg for variable, set it to expression
                let from_reg = self.expr_to_usable_once_reg();
                let var_reg = self.get_reg();
                self.varname_to_reg.insert(varidx, var_reg);
                mk_load_reg(self.s_i(), var_reg, from_reg);
                self.stack.push(StackItem::Unit);
            }
            Soken::Binop(binop) => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                // if both are numbers here, both will get their own temporary regs
                let right = self.realize_stackitem(right, true);
                let left = self.realize_stackitem(left, true);
                // then the temporaries will be 'freed' here
                let right = self.use_some_reg(right);
                let left = self.use_some_reg(left);
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
                        // WHY IN THE WHOLE WORLD WOULD I RETURN AN EXPRESSION HERE?
                        // it's gauranteed that nobody will use the result of a set-ing
                        self.stack.push(StackItem::Unit);
                    }
                    // both could be variables, so don't overwrite any of them
                    // create new register
                    B::Eql | B::Les | B::Mor | B::Add | B::Sub | B::Mul => {
                        let op = match binop {
                            B::Add => Op::Add,
                            B::Sub => Op::Sub,
                            B::Mul => Op::Mul,
                            B::Eql => Op::Eql,
                            B::Les => Op::Les,
                            B::Mor => Op::Mor,
                            _ => unreachable!(),
                        };
                        // have to allocate a tmp register for result
                        let reg = self.get_reg();
                        mk_op(self.e_i(), reg, op, left, right);
                        self.stack.push(StackItem::S(SomeReg::Tmp(reg)));
                    }
                }
            }
            Soken::FuncDef(name) => {
                let args = *self.functions.get(&name).unwrap();
                // use up X amount of registers
                for _ in 0..args {
                    let reg = self.get_reg();
                    let arg = self.eat();
                    if let Soken::RVar(ident) = arg {
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
                println!(
                    "watermark at: {:?}, reg at: {:?}",
                    self.reg_func_watermark, self.free_reg
                );
                self.func_array[func_idx.0 as usize].regs_used = self.reg_func_watermark;
                self.free_reg = 0;
                self.reg_func_watermark = 0;
                mk_end_func(self.s_i());
            }
            Soken::FuncCall(name, nr_args) => {
                let mut args = Vec::new();
                for _ in 0..nr_args {
                    let stackitem = self.stack.pop().unwrap();
                    let some_reg = self.realize_stackitem(stackitem, true);
                    args.push(some_reg);
                }
                // bc get of stack means they are: 3 2 1 0
                args.reverse();

                // potentially free some regs
                let args: Vec<_> = args.into_iter().map(|x| self.use_some_reg(x)).collect();

                // good
                let into_reg = self.get_reg();
                let new_name = self.ident_to_func_idx[&name];
                mk_call(self.e_i(), into_reg, new_name, &args);
                self.stack.push(StackItem::S(SomeReg::Tmp(into_reg)));
            }
            Soken::If => {
                let else_scope = self.scopes.pop().unwrap();
                let then_scope = self.scopes.pop().unwrap();

                let else_l = self.get_label();
                let end_l = self.get_label();

                let cond_reg = self.expr_to_usable_once_reg();
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
                let cond_reg = self.expr_to_usable_once_reg();
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
            Soken::EndScope => (),
            Soken::DropScope => {
                self.scopes.pop().unwrap();
            }
            Soken::Nil => unreachable!("no nils"),
        }
    }
}
