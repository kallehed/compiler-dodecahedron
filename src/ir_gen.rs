use crate::IdentIdx;

type Reg = u16;

enum Instr {
    Load(Reg, i16),
    Do(u16, u16, u16, u16),
}
