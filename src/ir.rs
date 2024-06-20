
use crate::lexer::IntIdx;
use crate::IdentIdx;

pub type Reg = u16;

/// in instructions we will have labels
/// TODO: could possibly have hashmap saying where on is
/// can jump to them
#[derive(Clone, Copy, Debug)]
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
pub struct IRFunc {
    pub params: u16,
    pub deadname: IdentIdx,
    pub regs_used: u16,
}

pub struct InstrIterator {
    bytecode: Vec<ByteCode>,
    at: usize,
}
/// take Vec of Bytecode, output nice Instr enum that you can match on 
impl InstrIterator {
    pub fn new(bytecode: Vec<ByteCode>) -> InstrIterator {
        InstrIterator {bytecode, at: 0}
    }
}

impl Iterator for InstrIterator {
    type Item = Instr;
    fn next(&mut self) -> Option<Self::Item> {
        if self.at >= self.bytecode.len() {return None;}

        let discriminator = self.bytecode[self.at];
        let ir_head = unsafe {std::mem::transmute(discriminator)};

        self.at += 1;
        // get's the next 2byte and transmutes it to type needed.
        macro_rules! e {
            () => {
                {
                    let ne = self.bytecode[self.at];
                    self.at += 1;
                    unsafe {std::mem::transmute(ne)}
                }
            }
        }
        let val = match ir_head {
            IRHead::LoadReg => Instr::LoadReg(e!(), e!()),
            IRHead::LoadInt => Instr::LoadInt(e!(), e!()),
            IRHead::Op => Instr::Op(e!(), e!(), e!(), e!()),
            IRHead::Call =>  Instr::Call(e!(), e!(), e!()),
            IRHead::Jump => Instr::Jump(e!()) ,
            IRHead::JumpRegZero => Instr::JumpRegZero(e!(), e!()) ,
            IRHead::Label => Instr::Label(e!()),
            IRHead::Return => Instr::Return(e!()) ,
            IRHead::FuncDef => Instr::FuncDef(e!()) ,
            IRHead::EndFunc =>  Instr::EndFunc, 
        };
        println!("return instr: {:?}", val);

        Some(val)
    }
}



/// High level overview of what an instruction is, will actually be represented as packed vec of ByteCode, returned in InstrIterator
#[derive(Clone, Copy, Debug)]
pub enum Instr {
    /// load value of one register into another
    LoadReg(Reg, Reg),
    /// load Int into register
    LoadInt(Reg, IntIdx),
    // .0 = .2 `.1` .3
    Op(Reg, Op, Reg, Reg),
    /// call .0 with register starting at .1 (look up how many args) put result into .2
    Call(FuncIdx, Reg, Reg),

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
