use crate::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum FrontendTypeKind {
    Identifier(String),
    Vector(Box<FrontendType>),
    Tuple(Vec<FrontendType>),
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FrontendType {
    pub kind: FrontendTypeKind,
    pub span: Span,
}