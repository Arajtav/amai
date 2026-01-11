use crate::common::*;
use super::ftypes::FrontendType;

#[derive(Debug, Clone)]
pub struct AmaiASTModule {
    pub path: String,
    pub nodes: Vec<ASTNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTNodeType {
    IntLit(i64),
    FloatLit(f64),
    Boolean(bool),
    Identifier(String),
    Semi(Box<ASTNode>),
    Tuple(Vec<ASTNode>),
    Block(Vec<ASTNode>),
    Unit,
    BinaryOp {
        op: Operator,
        lhs: Box<ASTNode>,
        rhs: Box<ASTNode>,
    },
    UnaryOp {
        op: Operator,
        operand: Box<ASTNode>,
    },
    LetDecl {
        name: String,
        ty: Option<FrontendType>,
        init: Option<Box<ASTNode>>,
    },
    VarDecl {
        name: String,
        ty: Option<FrontendType>,
        init: Option<Box<ASTNode>>,
    },
    If {
        condition: Box<ASTNode>,
        then_body: Box<ASTNode>,
        else_body: Option<Box<ASTNode>>,
    },
    While {
        condition: Box<ASTNode>,
        body: Box<ASTNode>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTNode {
    pub ty: ASTNodeType,
    pub span: Span,
}