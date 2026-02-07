pub mod arena;
pub mod call_frame;
pub mod function;
pub mod inst;
pub mod value;

use crate::{codegen::value::ValueBuilder, common::Span};
use arena::Arena;
use call_frame::CallFrame;
use function::Function;
use inst::Opcode;
use std::rc::Rc;
use value::Value;

pub type AmaiExtFn = Rc<dyn Fn(&mut AmaiVM, &[Value])>;

pub struct AmaiVM {
    pub frames: Vec<CallFrame>,
    pub constants: Vec<Value>,
    pub running: bool,
    pub functions: Vec<(Rc<Function>, [Value; 64])>,
    pub allow_large_bytecode: bool,
    pub external_functions: Vec<AmaiExtFn>,
    pub arena: Arena,
}

impl AmaiVM {
    pub fn new(allow_large_bytecode: bool) -> Self {
        Self {
            frames: Vec::new(),
            constants: Vec::new(),
            running: false,
            functions: Vec::new(),
            allow_large_bytecode,
            external_functions: Vec::new(),
            arena: Arena::new(),
        }
    }

    pub fn precompile_constants(&mut self, constants: &[ValueBuilder]) {
        let mut new_constants = Vec::new();
        for value in constants {
            if value.is_large() {
                let addr = self.arena.alloc(value.size(), value.align());
                self.arena.write(addr, &value.data());
                new_constants.push(Value::from_ptr(addr));
            } else {
                new_constants.push(value.to_value());
            }
        }
        self.constants = new_constants;
    }

    pub fn potentially_alloc(&mut self, values: &[ValueBuilder]) -> Vec<Value> {
        let mut new_vals = Vec::new();
        for value in values {
            if value.is_large() {
                let addr = self.arena.alloc(value.size(), value.align());
                self.arena.write(addr, &value.data());
                new_vals.push(Value::from_ptr(addr));
            } else {
                new_vals.push(value.to_value())
            }
        }
        new_vals
    }

    pub fn add_extern_fn<F: Fn(&mut AmaiVM, &[Value]) + 'static>(&mut self, f: F) -> usize {
        self.external_functions.push(Rc::new(f));
        self.external_functions.len() - 1
    }

    #[inline(always)]
    pub fn add_function(&mut self, bytecode: Box<[(u32, Span)]>, registers: &[Value]) -> usize {
        if !self.allow_large_bytecode {
            assert!(
                bytecode.len() < 65536,
                "Bytecode length is out of jump bounds"
            );
        }

        let func = Function { bytecode };
        self.functions
            .push((Rc::new(func), registers.try_into().unwrap()));
        self.functions.len() - 1
    }

    #[inline(always)]
    pub fn call_function(&mut self, id: usize, caller_args: Box<[Value]>) {
        let (function, registers) = self.functions[id].clone();
        let new_frame = CallFrame {
            caller_args,
            callee_args: Vec::new(),
            function,
            registers,
            ip: 0,
        };
        self.frames.push(new_frame);
    }
    #[inline(always)]
    pub fn call_external(&mut self, id: usize, caller_args: &[Value]) {
        let f = self.external_functions[id].clone();
        f(self, caller_args);
    }

    #[inline(always)]
    pub fn return_function(&mut self) {
        if let Some(mut callee_frame) = self.frames.pop()
            && let Some(caller_frame) = self.frames.last_mut()
        {
            caller_frame.registers[0] = callee_frame.callee_args.pop().unwrap_or(Value::nil());
        }
    }

    pub fn run(&mut self) -> Result<(), (String, Span)> {
        self.running = true;
        while self.running {
            unsafe { self.cycle()? }
        }

        Ok(())
    }

    #[inline(always)]
    pub fn cycle(&mut self) -> Result<(), (String, Span)> {
        if self.frames.is_empty() {
            self.running = false;
            return Ok(());
        }
        let frame = self.frames.last_mut().unwrap();
        let Some((inst, span)) = frame.function.bytecode.get(frame.ip) else {
            self.running = false;
            return Ok(());
        };
        let mut next_ip = frame.ip + 1;

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
                        {
                            let val = get_arg!($ty, shift);
                            shift += std::mem::size_of::<$ty>() * 8;
                            val
                        }
                    ),+
                )
            }};
        }

        let opcode = get_arg!(u8, 0);
        let opcode = Opcode::try_from(opcode).expect("Unknown opcode: {opcode:#04X}");

        macro_rules! reg_8_8_8_map {
            ($method:ident) => {{
                let (a, b, c) = decode_args!([u8, u8, u8]);
                let src1 = frame.registers[b as usize];
                let src2 = frame.registers[c as usize];
                frame.registers[a as usize] = src1.$method(src2).map_err(|err| (err, *span))?;
            }};
        }

        macro_rules! reg_8_8_8 {
            ($method:ident) => {{
                let (a, b, c) = decode_args!([u8, u8, u8]);
                let src1 = frame.registers[b as usize];
                let src2 = frame.registers[c as usize];
                frame.registers[a as usize] = src1.$method(src2);
            }};
        }

        macro_rules! reg_8_16 {
            ($method:ident) => {{
                let (a, b) = decode_args!([u8, u16]);
                let src = frame.registers[b as usize];
                frame.registers[a as usize] = src.$method();
            }};
        }

        match opcode {
            Opcode::Nop => {}
            Opcode::Load => {
                let (dest, id) = decode_args!([u8, u16]);
                frame.registers[dest as usize] = self.constants[id as usize];
            }
            Opcode::IAdd => reg_8_8_8_map!(iadd),
            Opcode::ISub => reg_8_8_8_map!(isub),
            Opcode::IMul => reg_8_8_8_map!(imul),
            Opcode::IDiv => reg_8_8_8_map!(idiv),
            Opcode::IRem => reg_8_8_8_map!(irem),
            Opcode::FAdd => reg_8_8_8!(fadd),
            Opcode::FSub => reg_8_8_8!(fsub),
            Opcode::FMul => reg_8_8_8!(fmul),
            Opcode::FDiv => {
                let src1 = frame.registers[((inst >> 16) & 0xFF) as usize];
                let src2 = frame.registers[((inst >> 24) & 0xFF) as usize];

                frame.registers[((inst >> 8) & 0xFF) as usize] = src1
                    .fdiv(src2)
                    .ok_or(("Division by zero".to_owned(), *span))?;
            }
            Opcode::FRem => {
                let src1 = frame.registers[((inst >> 16) & 0xFF) as usize];
                let src2 = frame.registers[((inst >> 24) & 0xFF) as usize];

                frame.registers[((inst >> 8) & 0xFF) as usize] = src1
                    .frem(src2)
                    .ok_or(("Division by zero".to_owned(), *span))?;
            }
            Opcode::BOr => reg_8_8_8!(bor),
            Opcode::BAnd => reg_8_8_8!(band),
            Opcode::BXor => reg_8_8_8!(bxor),
            Opcode::BNot => {
                let src = frame.registers[((inst >> 16) & 0xFF) as usize];

                frame.registers[((inst >> 8) & 0xFF) as usize] = src.bnot();
            }
            Opcode::LOr => reg_8_8_8!(lor),
            Opcode::LAnd => reg_8_8_8!(land),
            Opcode::LNot => {
                let src = frame.registers[((inst >> 16) & 0xFF) as usize];

                frame.registers[((inst >> 8) & 0xFF) as usize] = src.lnot();
            }
            Opcode::CMEq => reg_8_8_8!(cmeq),
            Opcode::CMNE => reg_8_8_8!(cmne),
            Opcode::ICGT => reg_8_8_8!(icgt),
            Opcode::ICLT => reg_8_8_8!(iclt),
            Opcode::ICGE => reg_8_8_8!(icge),
            Opcode::ICLE => reg_8_8_8!(icle),
            Opcode::FCGT => reg_8_8_8!(fcgt),
            Opcode::FCLT => reg_8_8_8!(fclt),
            Opcode::FCGE => reg_8_8_8!(fcge),
            Opcode::FCLE => reg_8_8_8!(fcle),
            Opcode::Jump => {
                let addr = get_arg!(i16, 8);

                if addr >= 0 {
                    next_ip = frame.ip + addr.cast_unsigned() as usize;
                } else {
                    next_ip = frame.ip - addr.unsigned_abs() as usize;
                }
            }
            Opcode::JITr => {
                let addr = get_arg!(i16, 8);
                let src = frame.registers[get_arg!(u8, 24) as usize].to_bool();

                if src {
                    if addr >= 0 {
                        next_ip = frame.ip + addr.cast_unsigned() as usize;
                    } else {
                        next_ip = frame.ip - addr.unsigned_abs() as usize;
                    }
                }
            }
            Opcode::JIFl => {
                let addr = get_arg!(i16, 8);
                let src = frame.registers[get_arg!(u8, 24) as usize].to_bool();

                if !src {
                    if addr >= 0 {
                        next_ip = frame.ip + addr.cast_unsigned() as usize;
                    } else {
                        next_ip = frame.ip - addr.unsigned_abs() as usize;
                    }
                }
            }
            Opcode::Call => {
                let ptr = frame.registers[get_arg!(u8, 8) as usize].to_ptr();
                let args = std::mem::take(&mut frame.callee_args).into_boxed_slice();
                frame.ip = next_ip;
                self.call_function(ptr, args);
                return Ok(());
            }
            Opcode::Retn => {
                self.return_function();
                return Ok(());
            }
            Opcode::INeg => reg_8_16!(ineg),
            Opcode::FNeg => reg_8_16!(fneg),
            Opcode::Move => {
                let src = frame.registers[((inst >> 16) & 0xFF) as usize];

                frame.registers[((inst >> 8) & 0xFF) as usize] = src;
            }
            Opcode::PArg => {
                let src = frame.registers[((inst >> 8) & 0xFF) as usize];

                frame.callee_args.push(src);
            }
            Opcode::CArg => {
                let arg_id = ((inst >> 16) & 0xFFFF) as usize;

                frame.registers[((inst >> 8) & 0xFF) as usize] = frame.caller_args[arg_id];
            }
            Opcode::CExt => {
                let id = (inst >> 8) & 0x00FF_FFFF;

                let args = std::mem::take(&mut frame.callee_args);
                frame.ip = next_ip;
                self.call_external(id as usize, &args);
                return Ok(());
            }
            Opcode::LShf => reg_8_8_8_map!(lshf),
            Opcode::RShf => reg_8_8_8_map!(rshf),
            Opcode::SCon => {
                let src1 = frame.registers[((inst >> 16) & 0xFF) as usize];
                let src2 = frame.registers[((inst >> 24) & 0xFF) as usize];

                frame.registers[((inst >> 8) & 0xFF) as usize] = src1.scon(src2, &mut self.arena);
            }
            Opcode::SCEq => {
                let src1 = frame.registers[((inst >> 16) & 0xFF) as usize];
                let src2 = frame.registers[((inst >> 24) & 0xFF) as usize];

                frame.registers[((inst >> 8) & 0xFF) as usize] = src1.sceq(src2, &mut self.arena);
            }
            Opcode::SCNE => {
                let src1 = frame.registers[((inst >> 16) & 0xFF) as usize];
                let src2 = frame.registers[((inst >> 24) & 0xFF) as usize];

                frame.registers[((inst >> 8) & 0xFF) as usize] = src1.scne(src2, &mut self.arena);
            }
            Opcode::Halt => self.running = false,
        }

        frame.ip = next_ip;

        Ok(())
    }
}
