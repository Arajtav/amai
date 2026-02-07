use crate::common::Span;

#[derive(Clone)]
pub struct Function {
    pub bytecode: Box<[(u32, Span)]>,
}
