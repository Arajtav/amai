use num_enum::TryFromPrimitive;
use strum_macros::AsRefStr;

#[repr(u8)]
#[derive(TryFromPrimitive, AsRefStr)]
pub enum Opcode {
    NOP = 0x00,
    LOAD = 0x01,
    IADD = 0x02,
    ISUB = 0x03,
    IMUL = 0x04,
    IDIV = 0x05,
    IREM = 0x06,
    FADD = 0x07,
    FSUB = 0x08,
    FMUL = 0x09,
    FDIV = 0x0A,
    FREM = 0x0B,
    BOR = 0x0C,
    BAND = 0x0D,
    BXOR = 0x0E,
    BNOT = 0x0F,
    LOR = 0x10,
    LAND = 0x11,
    LNOT = 0x12,
    CMEQ = 0x13,
    CMNE = 0x14,
    ICGT = 0x15,
    ICLT = 0x16,
    ICGE = 0x17,
    ICLE = 0x18,
    FCGT = 0x19,
    FCLT = 0x1A,
    FCGE = 0x1B,
    FCLE = 0x1C,
    JUMP = 0x1D, // NOTE: the destination operand (register) will be read as an instruction index
    JITR = 0x1E, // jump if true
    JIFL = 0x1F, // jump if false
    CALL = 0x20,
    RETN = 0x21,
    INEG = 0x22,
    FNEG = 0x23,
    MOVE = 0x24,
    PARG = 0x25, // push arg (to caller args)
    CARG = 0x26, // copy arg (from caller args)
    CEXT = 0x27, // call external
    LSHF = 0x28,
    RSHF = 0x29,
    SCON = 0x2A,
    SCEQ = 0x2B,
    SCNE = 0x2C,
    HALT = 0xFF,
}
