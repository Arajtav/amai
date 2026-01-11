use crate::common::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum PatternLiteral {
    Integer(i64),
    Float(f64),
    Boolean(bool),
}

#[derive(Debug, Clone, PartialEq)]
pub enum PatternType {
    Identifier(String),
    Literal(PatternLiteral),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Pattern {
    pub ty: PatternType,
    pub span: Span,
}