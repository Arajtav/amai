use std::collections::HashMap;

use amai_vm::{isa::*, value::*};
use crate::{operator::Operator, parser::ast::*};

pub struct BytecodeGenerator {
    ast: AmaiASTModule,
    pub constants: Vec<Value>,
    pub locals: HashMap<String, usize>,
}

impl BytecodeGenerator {
    pub fn new(ast: AmaiASTModule) -> BytecodeGenerator {
        BytecodeGenerator {
            ast,
            constants: Vec::new(),
            locals: HashMap::new(),
        }
    }

    pub fn generate(&mut self) -> Vec<u8> {
        let mut bytes = Vec::new();

        for node in self.ast.nodes.clone() {
            self.compile(&node, &mut bytes);
        }
        bytes.push(OP_HALT);

        while self.constants.len() < 65_535 {
            self.constants.push(Value::Nil);
        }

        bytes
    }

    pub fn compile(&mut self, node: &AmaiASTNode, bytes: &mut Vec<u8>) {
        match &node.kind {
            AmaiASTNodeKind::IntLit(i) => {
                let int = Value::Int(*i);
                if !self.constants.contains(&int) {
                    self.constants.push(int);
                }
                let const_idx = self.constants
                    .iter()
                    .position(|s| *s == Value::Int(*i))
                    .unwrap() as u16;
                let low = (const_idx & 0xFF) as u8;
                let high = ((const_idx >> 8) & 0xFF) as u8;
                bytes.extend(&[OP_PUSH, low, high]);
            },
            AmaiASTNodeKind::FloatLit(i) => {
                let int = Value::Float(*i);
                if !self.constants.contains(&int) {
                    self.constants.push(int);
                }
                let const_idx = self.constants
                    .iter()
                    .position(|s| *s == Value::Float(*i))
                    .unwrap() as u16;
                let low = (const_idx & 0xFF) as u8;
                let high = ((const_idx >> 8) & 0xFF) as u8;
                bytes.extend(&[OP_PUSH, low, high]);
            },
            AmaiASTNodeKind::Boolean(i) => {
                let int = Value::Bool(*i);
                if !self.constants.contains(&int) {
                    self.constants.push(int);
                }
                let const_idx = self.constants
                    .iter()
                    .position(|s| *s == Value::Bool(*i))
                    .unwrap() as u16;
                let low = (const_idx & 0xFF) as u8;
                let high = ((const_idx >> 8) & 0xFF) as u8;
                bytes.extend(&[OP_PUSH, low, high]);
            },
            AmaiASTNodeKind::BinaryOp { op, lhs, rhs } => {
                self.compile(&lhs, bytes);
                self.compile(&rhs, bytes);
                match *op {
                    Operator::Plus => bytes.push(OP_ADD),
                    Operator::Minus => bytes.push(OP_SUB),
                    Operator::Star => bytes.push(OP_MUL),
                    Operator::Slash => bytes.push(OP_DIV),
                    Operator::Eq => bytes.push(OP_CMEQ),
                    Operator::Ne => bytes.push(OP_CMNE),
                    Operator::Gt => bytes.push(OP_CMGT),
                    Operator::Lt => bytes.push(OP_CMLT),
                    Operator::Ge => bytes.push(OP_CMGE),
                    Operator::Le => bytes.push(OP_CMLE),
                    Operator::Concat => bytes.push(OP_CONC),
                    Operator::LogOr => bytes.push(OP_LOR),
                    Operator::LogAnd => bytes.push(OP_LAND),
                    Operator::Pipe => bytes.push(OP_BOR),
                    Operator::Ampersand => bytes.push(OP_BAND),
                    Operator::Caret => bytes.push(OP_BXOR),
                    Operator::Lsh => bytes.push(OP_LSH),
                    Operator::LRsh => bytes.push(OP_LRSH),
                    Operator::ARsh => bytes.push(OP_ARSH),
                    _ => todo!("OPERATOR IN BINARY OP: {op:?}"),
                }
            },
            _ => todo!()
        }
    }
}