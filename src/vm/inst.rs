use num_enum::TryFromPrimitive;
use strum_macros::AsRefStr;

#[repr(u8)]
#[derive(TryFromPrimitive, AsRefStr)]
#[strum(serialize_all = "UPPERCASE")]
#[allow(clippy::upper_case_acronyms)]
pub enum Opcode {
    Nop = 0x00,

    Load = 0x01,

    IAdd = 0x02,
    ISub = 0x03,
    IMul = 0x04,
    IDiv = 0x05,
    IRem = 0x06,

    FAdd = 0x07,
    FSub = 0x08,
    FMul = 0x09,
    FDiv = 0x0A,
    FRem = 0x0B,

    BOr = 0x0C,
    BAnd = 0x0D,
    BXor = 0x0E,
    BNot = 0x0F,

    LOr = 0x10,
    LAnd = 0x11,
    LNot = 0x12,

    CMEq = 0x13,
    CMNE = 0x14,

    ICGT = 0x15,
    ICLT = 0x16,
    ICGE = 0x17,
    ICLE = 0x18,

    FCGT = 0x19,
    FCLT = 0x1A,
    FCGE = 0x1B,
    FCLE = 0x1C,

    Jump = 0x1D, // NOTE: the destination operand (register) will be read as an instruction index
    JITr = 0x1E, // jump if true
    JIFl = 0x1F, // jump if false

    Call = 0x20,
    Retn = 0x21,

    INeg = 0x22,
    FNeg = 0x23,

    Move = 0x24,

    PArg = 0x25, // push arg (to caller args)
    CArg = 0x26, // copy arg (from caller args)

    LShf = 0x28,
    RShf = 0x29,

    SCon = 0x2A,
    SCEq = 0x2B,
    SCNE = 0x2C,

    Halt = 0xFF,
}
