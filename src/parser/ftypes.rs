use crate::common::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum FrontendTypeType {
    Identifier(String),
    Vector(Box<FrontendType>),
    Tuple(Vec<FrontendType>),
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FrontendType {
    pub ty: FrontendTypeType,
    pub span: Span,
}