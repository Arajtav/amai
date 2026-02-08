use crate::vm::inst::Opcode;
use std::fmt::Write as _;

pub fn disassemble(bytecode: &[u32]) -> String {
    let mut output = String::new();
    for inst in bytecode {
        macro_rules! get_arg {
            ($ty:ty, $pos:expr) => {{
                const LEN: usize = std::mem::size_of::<$ty>() * 8;
                const MASK: u32 = if LEN == 32 {
                    u32::MAX
                } else {
                    (1u32 << LEN) - 1
                };
                ((inst >> $pos) & MASK) as $ty
            }};
        }

        macro_rules! decode_args {
            ([$($ty:ty),+ $(,)?]) => {{
                let mut shift = 8;
                (
                    $(
                        #[allow(unused_assignments)]
                        {
                            let val = get_arg!($ty, shift);
                            shift += std::mem::size_of::<$ty>() * 8;
                            val
                        }
                    ),+
                )
            }};
        }

        let Ok(opcode) = Opcode::try_from(get_arg!(u8, 0)) else {
            todo!()
        };

        output.push_str(opcode.as_ref());

        macro_rules! write_out {
            ($($arg:tt)*) => {
                let _ = write!(output, $($arg)*);
            };
        }

        match opcode {
            Opcode::Load | Opcode::CArg => {
                let (dest, const_id) = decode_args!([u8, u16]);
                write_out!(" r{dest} #{const_id}");
            }
            Opcode::IAdd
            | Opcode::ISub
            | Opcode::IMul
            | Opcode::IDiv
            | Opcode::IRem
            | Opcode::FAdd
            | Opcode::FSub
            | Opcode::FMul
            | Opcode::FDiv
            | Opcode::FRem
            | Opcode::BOr
            | Opcode::BAnd
            | Opcode::BXor
            | Opcode::LOr
            | Opcode::LAnd
            | Opcode::CMEq
            | Opcode::CMNE
            | Opcode::ICGT
            | Opcode::ICLT
            | Opcode::ICGE
            | Opcode::ICLE
            | Opcode::FCGT
            | Opcode::FCLT
            | Opcode::FCGE
            | Opcode::FCLE
            | Opcode::LShf
            | Opcode::RShf
            | Opcode::SCon
            | Opcode::SCEq
            | Opcode::SCNE => {
                let (dest, src1, src2) = decode_args!([u8, u8, u8]);
                write_out!(" r{dest} r{src1} r{src2}");
            }
            Opcode::BNot | Opcode::LNot | Opcode::INeg | Opcode::FNeg | Opcode::Move => {
                let (dest, src1) = decode_args!([u8, u8]);
                write_out!(" r{dest} r{src1}");
            }
            Opcode::Jump => {
                let a = get_arg!(i16, 8);
                let dest = if a >= 0 {
                    format!("+{a}")
                } else {
                    a.to_string()
                };
                write_out!(" {dest}");
            }
            Opcode::JITr | Opcode::JIFl => {
                let (a, src1) = decode_args!([i16, u8]);
                let dest = if a >= 0 {
                    format!("+{a}")
                } else {
                    a.to_string()
                };
                write_out!(" {dest} r{src1}");
            }
            Opcode::Call => {
                let dest = get_arg!(u8, 8);
                write_out!(" r{dest}");
            }
            Opcode::PArg => {
                let src = get_arg!(u8, 8);
                write_out!(" r{src}");
            }
            Opcode::Nop | Opcode::Halt | Opcode::Retn => {}
        }
        output.push('\n');
    }

    output
}
