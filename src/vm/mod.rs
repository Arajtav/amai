pub mod value;
pub mod inst;

use std::rc::Rc;
use value::*;
use inst::*;

#[derive(Clone)]
pub struct AmaiVM<'vm> {
    pub frames: Vec<CallFrame<'vm>>,
    pub constants: &'vm [Value],
    pub running: bool,
    pub functions: Vec<Rc<Function<'vm>>>,
}

impl<'vm> AmaiVM<'vm> {
    pub fn new(constants: &'vm [Value]) -> Self {
        Self {
            frames: Vec::new(),
            constants,
            running: false,
            functions: Vec::new(),
        }
    }

    
    #[inline(always)]
    pub fn add_function(&mut self, bytecode: &'vm [u8], constant_count: usize) {
        let func = Function { constant_count, bytecode };
        self.functions.push(Rc::new(func));
    }

    #[inline(always)]
    pub fn call_function(&mut self, id: usize) {
        let func = self.functions[id].clone();
        let ip = func.bytecode.as_ptr();
        let new_frame = CallFrame {
            function: func,
            registers: [Value::nil(); 256],
            constant_idx_base: self.frames
                .last()
                .map(|f|
                    f.constant_idx_base + f.function.constant_count
                ).unwrap_or(0),
            bytecode_base: ip,
            ip,
        };
        self.frames.push(new_frame);
    }

    #[inline(always)]
    pub fn return_function(&mut self) {
        self.frames.pop();
    }

    pub fn run(&mut self) -> Result<(), &'static str> {
        self.running = true;
        while self.running {
            unsafe { self.cycle()? }
        }

        Ok(())
    }

    #[inline(always)]
    #[allow(unsafe_op_in_unsafe_fn)]
    pub unsafe fn cycle(&mut self) -> Result<(), &'static str> {
        let frame = self.frames.as_mut_ptr().add(self.frames.len() - 1);
        let opcode = *(*frame).ip;
        let next_ip = (*frame).ip.add(4);

        match opcode {
            NOP => {},
            LOAD => {
                let dest = *(*frame).ip.add(1);
                let id = *(*frame).ip.add(2) as u16
                    | (( *(*frame).ip.add(3) as u16 ) << 8);
                let abs_idx = (*frame).constant_idx_base + id as usize;
                let constant = *self.constants.get_unchecked(abs_idx);

                (*frame).registers[dest as usize] = constant;
            },
            IADD => {
                let dest = *(*frame).ip.add(1);
                let src1 = (*frame).registers[ *(*frame).ip.add(2) as usize ];
                let src2 = (*frame).registers[ *(*frame).ip.add(3) as usize ];

                (*frame).registers[dest as usize] = src1.iadd(src2);
            },
            ISUB => {
                let dest = *(*frame).ip.add(1);
                let src1 = (*frame).registers[ *(*frame).ip.add(2) as usize ];
                let src2 = (*frame).registers[ *(*frame).ip.add(3) as usize ];

                (*frame).registers[dest as usize] = src1.isub(src2);
            },
            IMUL => {
                let dest = *(*frame).ip.add(1);
                let src1 = (*frame).registers[ *(*frame).ip.add(2) as usize ];
                let src2 = (*frame).registers[ *(*frame).ip.add(3) as usize ];

                (*frame).registers[dest as usize] = src1.imul(src2);
            },
            IDIV => {
                let dest = *(*frame).ip.add(1);
                let src1 = (*frame).registers[ *(*frame).ip.add(2) as usize ];
                let src2 = (*frame).registers[ *(*frame).ip.add(3) as usize ];

                (*frame).registers[dest as usize] = src1.idiv(src2).ok_or("Division by zero")?;
            },
            IREM => {
                let dest = *(*frame).ip.add(1);
                let src1 = (*frame).registers[ *(*frame).ip.add(2) as usize ];
                let src2 = (*frame).registers[ *(*frame).ip.add(3) as usize ];

                (*frame).registers[dest as usize] = src1.irem(src2).ok_or("Division by zero")?;
            },
            FADD => {
                let dest = *(*frame).ip.add(1);
                let src1 = (*frame).registers[ *(*frame).ip.add(2) as usize ];
                let src2 = (*frame).registers[ *(*frame).ip.add(3) as usize ];

                (*frame).registers[dest as usize] = src1.fadd(src2);
            },
            FSUB => {
                let dest = *(*frame).ip.add(1);
                let src1 = (*frame).registers[ *(*frame).ip.add(2) as usize ];
                let src2 = (*frame).registers[ *(*frame).ip.add(3) as usize ];

                (*frame).registers[dest as usize] = src1.fsub(src2);
            },
            FMUL => {
                let dest = *(*frame).ip.add(1);
                let src1 = (*frame).registers[ *(*frame).ip.add(2) as usize ];
                let src2 = (*frame).registers[ *(*frame).ip.add(3) as usize ];

                (*frame).registers[dest as usize] = src1.fmul(src2);
            },
            FDIV => {
                let dest = *(*frame).ip.add(1);
                let src1 = (*frame).registers[ *(*frame).ip.add(2) as usize ];
                let src2 = (*frame).registers[ *(*frame).ip.add(3) as usize ];

                (*frame).registers[dest as usize] = src1.fdiv(src2).ok_or("Division by zero")?;
            },
            FREM => {
                let dest = *(*frame).ip.add(1);
                let src1 = (*frame).registers[ *(*frame).ip.add(2) as usize ];
                let src2 = (*frame).registers[ *(*frame).ip.add(3) as usize ];

                (*frame).registers[dest as usize] = src1.frem(src2).ok_or("Division by zero")?;
            },
            BOR => {
                let dest = *(*frame).ip.add(1);
                let src1 = (*frame).registers[ *(*frame).ip.add(2) as usize ];
                let src2 = (*frame).registers[ *(*frame).ip.add(3) as usize ];

                (*frame).registers[dest as usize] = src1.bor(src2);
            },
            BAND => {
                let dest = *(*frame).ip.add(1);
                let src1 = (*frame).registers[ *(*frame).ip.add(2) as usize ];
                let src2 = (*frame).registers[ *(*frame).ip.add(3) as usize ];

                (*frame).registers[dest as usize] = src1.band(src2);
            },
            BXOR => {
                let dest = *(*frame).ip.add(1);
                let src1 = (*frame).registers[ *(*frame).ip.add(2) as usize ];
                let src2 = (*frame).registers[ *(*frame).ip.add(3) as usize ];

                (*frame).registers[dest as usize] = src1.bxor(src2);
            },
            BNOT => {
                let dest = *(*frame).ip.add(1) ;
                let src = (*frame).registers[ *(*frame).ip.add(2) as usize ];

                (*frame).registers[dest as usize] = src.bnot();
            },
            LOR => {
                let dest = *(*frame).ip.add(1);
                let src1 = (*frame).registers[ *(*frame).ip.add(2) as usize ];
                let src2 = (*frame).registers[ *(*frame).ip.add(3) as usize ];

                (*frame).registers[dest as usize] = src1.lor(src2);
            },
            LAND => {
                let dest = *(*frame).ip.add(1);
                let src1 = (*frame).registers[ *(*frame).ip.add(2) as usize ];
                let src2 = (*frame).registers[ *(*frame).ip.add(3) as usize ];

                (*frame).registers[dest as usize] = src1.land(src2);
            },
            LNOT => {
                let dest = *(*frame).ip.add(1);
                let src = (*frame).registers[ *(*frame).ip.add(2) as usize ];

                (*frame).registers[dest as usize] = src.lnot();
            },
            CMEQ => {
                let dest = *(*frame).ip.add(1);
                let src1 = (*frame).registers[ *(*frame).ip.add(2) as usize ];
                let src2 = (*frame).registers[ *(*frame).ip.add(3) as usize ];

                (*frame).registers[dest as usize] = src1.cmeq(src2);
            },
            CMNE => {
                let dest = *(*frame).ip.add(1);
                let src1 = (*frame).registers[ *(*frame).ip.add(2) as usize ];
                let src2 = (*frame).registers[ *(*frame).ip.add(3) as usize ];

                (*frame).registers[dest as usize] = src1.cmne(src2);
            },
            ICGT => {
                let dest = *(*frame).ip.add(1);
                let src1 = (*frame).registers[ *(*frame).ip.add(2) as usize ];
                let src2 = (*frame).registers[ *(*frame).ip.add(3) as usize ];

                (*frame).registers[dest as usize] = src1.icgt(src2);
            },
            ICLT => {
                let dest = *(*frame).ip.add(1);
                let src1 = (*frame).registers[ *(*frame).ip.add(2) as usize ];
                let src2 = (*frame).registers[ *(*frame).ip.add(3) as usize ];

                (*frame).registers[dest as usize] = src1.iclt(src2);
            },
            ICGE => {
                let dest = *(*frame).ip.add(1);
                let src1 = (*frame).registers[ *(*frame).ip.add(2) as usize ];
                let src2 = (*frame).registers[ *(*frame).ip.add(3) as usize ];

                (*frame).registers[dest as usize] = src1.icge(src2);
            },
            ICLE => {
                let dest = *(*frame).ip.add(1);
                let src1 = (*frame).registers[ *(*frame).ip.add(2) as usize ];
                let src2 = (*frame).registers[ *(*frame).ip.add(3) as usize ];

                (*frame).registers[dest as usize] = src1.icle(src2);
            },
            FCGT => {
                let dest = *(*frame).ip.add(1);
                let src1 = (*frame).registers[ *(*frame).ip.add(2) as usize ];
                let src2 = (*frame).registers[ *(*frame).ip.add(3) as usize ];

                (*frame).registers[dest as usize] = src1.fcgt(src2);
            },
            FCLT => {
                let dest = *(*frame).ip.add(1);
                let src1 = (*frame).registers[ *(*frame).ip.add(2) as usize ];
                let src2 = (*frame).registers[ *(*frame).ip.add(3) as usize ];

                (*frame).registers[dest as usize] = src1.fclt(src2);
            },
            FCGE => {
                let dest = *(*frame).ip.add(1);
                let src1 = (*frame).registers[ *(*frame).ip.add(2) as usize ];
                let src2 = (*frame).registers[ *(*frame).ip.add(3) as usize ];

                (*frame).registers[dest as usize] = src1.fcge(src2);
            },
            FCLE => {
                let dest = *(*frame).ip.add(1);
                let src1 = (*frame).registers[ *(*frame).ip.add(2) as usize ];
                let src2 = (*frame).registers[ *(*frame).ip.add(3) as usize ];

                (*frame).registers[dest as usize] = src1.fcle(src2);
            },
            JUMP => {
                let addr = (*(*frame).ip.add(1) as u16
                    | (( *(*frame).ip.add(2) as u16 ) << 8))
                    as i16;

                if addr >= 0 {
                    (*frame).ip = (*frame).bytecode_base.add(addr as usize * 4);
                } else {
                    (*frame).ip = (*frame).bytecode_base.sub(addr.abs() as usize * 4);
                }
            },
            JITR => {
                let addr = (*(*frame).ip.add(1) as u16
                    | (( *(*frame).ip.add(2) as u16 ) << 8))
                    as i16;
                let src = (*frame).registers[ *(*frame).ip.add(3) as usize ].to_bool();

                if src {
                    if addr >= 0 {
                    (*frame).ip = (*frame).bytecode_base.add(addr as usize * 4);
                    } else {
                        (*frame).ip = (*frame).bytecode_base.sub(addr.abs() as usize * 4);
                    }
                }
            },
            JIFL => {
                let addr = (*(*frame).ip.add(1) as u16
                    | (( *(*frame).ip.add(2) as u16 ) << 8))
                    as i16;
                let src = (*frame).registers[ *(*frame).ip.add(3) as usize ].to_bool();

                if !src {
                    if addr >= 0 {
                    (*frame).ip = (*frame).bytecode_base.add(addr as usize * 4);
                    } else {
                        (*frame).ip = (*frame).bytecode_base.sub(addr.abs() as usize * 4);
                    }
                }
            },
            CALL => {
                let dest = *(*frame).ip.add(1);
                let id = (*frame).registers[dest as usize].to_int() as usize;
                self.call_function(id);
            },
            RETN => self.return_function(),
            HALT => self.running = false,
            _ => todo!(),
        }

        (*frame).ip = next_ip;
        Ok(())
    }
}

#[derive(Clone)]
pub struct CallFrame<'cf> {
    pub function: Rc<Function<'cf>>,
    pub registers: [Value; 256],
    pub constant_idx_base: usize,
    pub bytecode_base: *const u8,
    pub ip: *const u8,
}

#[allow(unused)]
#[derive(Clone, Copy)]
pub struct Function<'func> {
    pub constant_count: usize,
    pub bytecode: &'func [u8],
}