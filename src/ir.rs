use crate::lexer::IntIdx;
use crate::IdentIdx;

pub type Reg = u16;

/// in instructions we will have labels
/// TODO: could possibly have hashmap saying where on is
/// can jump to them
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct Label(pub u16);

/// index into func_array
#[derive(Clone, Copy, Debug)]
pub struct FuncIdx(pub u16);

/// op used in IR, simple enumeration
#[derive(Clone, Copy, Debug)]
#[repr(u16)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Mor,
    Les,
    Eql,
}

/// Because Instr is made up of chunks of u16, where if you look at the one you can know the structure of some after, we can optimize it's array layout by squishing everything into an array, this removes indexability though Hopefully we won't need to index the bytecode instr, as we mostly iterate over them
pub type ByteCode = u16;

/// simple enumuration of different headers for IR instructions
#[derive(Clone, Copy, Debug)]
#[repr(u16)]
pub enum IRHead {
    LoadReg,
    LoadInt,
    Op,
    Call,

    Jump,
    JumpRegZero,
    Label,
    Return,
    FuncDef,
    EndFunc,
}

/// data for a function in IR, gotten by indexing into func_array array
#[derive(Debug)]
pub struct IRFunc {
    pub params: u16,
    pub deadname: IdentIdx,
    pub regs_used: u16,
}
/// lots of functions here for generating IR
pub fn mk_load_reg(s: &mut Vec<ByteCode>, r1: Reg, r2: Reg) {
    s.extend(&[IRHead::LoadReg as _, r1 as _, r2 as _]);
}
pub fn mk_load_int(s: &mut Vec<ByteCode>, r1: Reg, intidx: IntIdx) {
    s.extend(&[IRHead::LoadInt as _, r1 as _, unsafe {
        std::mem::transmute(intidx)
    }]);
}
pub fn mk_op(s: &mut Vec<ByteCode>, r1: Reg, op: Op, r2: Reg, r3: Reg) {
    s.extend(&[IRHead::Op as _, r1 as _, op as _, r2 as _, r3 as _]);
}
/// TODO: maybe take in reversed slice? We can then iterate backwards, bc of ir_gen.rs and Sokens
pub fn mk_call(s: &mut Vec<ByteCode>, to: Reg, fnidx: FuncIdx, args: &[Reg]) {
    s.extend(&[IRHead::Call as _, to as _, fnidx.0 as _]);
    for &arg in args.iter() {
        s.push(arg as _);
    }
}
pub fn mk_jump(s: &mut Vec<ByteCode>, label: Label) {
    s.extend(&[IRHead::Jump as _, label.0]);
}
pub fn mk_jump_req_zero(s: &mut Vec<ByteCode>, r1: Reg, label: Label) {
    s.extend(&[IRHead::JumpRegZero as _, r1 as _, label.0])
}
pub fn mk_label(s: &mut Vec<ByteCode>, label: Label) {
    s.extend(&[IRHead::Label as _, label.0])
}
pub fn mk_return(s: &mut Vec<ByteCode>, r1: Reg) {
    s.extend(&[IRHead::Return as _, r1 as _])
}
pub fn mk_func_def(s: &mut Vec<ByteCode>, fnidx: FuncIdx) {
    s.extend(&[IRHead::FuncDef as _, fnidx.0])
}
pub fn mk_end_func(s: &mut Vec<ByteCode>) {
    s.push(IRHead::EndFunc as _);
}

/// High level overview of what an instruction is, will actually be represented as packed vec of ByteCode, returned in InstrIterator
#[derive(Clone, Copy, Debug)]
pub enum Instr<'a> {
    /// load value of one register into another
    LoadReg(Reg, Reg),
    /// load Int into register
    LoadInt(Reg, IntIdx),
    /// .0 = .2 `.1` .3
    Op(Reg, Op, Reg, Reg),
    /// .0 = .1(.2...)        Call .1 with slice as arguments
    Call(Reg, FuncIdx, &'a [Reg]),

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
}
/// iterates nicely over bytecode, returning Instr
pub struct InstrIterator<'a> {
    bytecode: &'a Vec<ByteCode>,
    /// ONLY INTERPRETER IS ALLOWED TO ACCESS THIS
    pub at: usize,
    ir_functions: &'a [IRFunc],
}
/// take Vec of Bytecode, output nice Instr enum that you can match on
impl<'a> InstrIterator<'a> {
    pub fn new(bytecode: &'a Vec<ByteCode>, ir_functions: &'a [IRFunc]) -> InstrIterator<'a> {
        InstrIterator {
            bytecode,
            at: 0,
            ir_functions,
        }
    }
    pub fn next(&mut self) -> Option<Instr> {
        if self.at >= self.bytecode.len() {
            return None;
        }
        // get's the next 2byte and transmutes it to type needed.
        macro_rules! e {
            () => {{
                let ne = self.bytecode[self.at];
                self.at += 1;
                unsafe { std::mem::transmute(ne) }
            }};
        }
        Some(match e!() {
            IRHead::LoadReg => Instr::LoadReg(e!(), e!()),
            IRHead::LoadInt => Instr::LoadInt(e!(), e!()),
            IRHead::Op => Instr::Op(e!(), e!(), e!(), e!()),
            IRHead::Call => {
                let (res_reg, fn_idx): (Reg, FuncIdx) = (e!(), e!());
                let params: usize = self.ir_functions[fn_idx.0 as usize].params.into();
                let slic = &self.bytecode[self.at..(self.at + params)];
                self.at += params;
                Instr::Call(res_reg, fn_idx, slic)
            }
            IRHead::Jump => Instr::Jump(e!()),
            IRHead::JumpRegZero => Instr::JumpRegZero(e!(), e!()),
            IRHead::Label => Instr::Label(e!()),
            IRHead::Return => Instr::Return(e!()),
            IRHead::FuncDef => Instr::FuncDef(e!()),
            IRHead::EndFunc => Instr::EndFunc,
        })
    }
}
