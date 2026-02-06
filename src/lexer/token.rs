use crate::common::{Operator, Span};
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenType {
    IntLit(i64),
    FloatLit(f64),
    StringLit,
    Operator(Operator),
    Identifier,
    Let,
    If,
    Else,
    While,
    For,
    In,
    Return,
    Extern,
    Export,
    Do,
    Then,
    With,
    True,
    False,
    LParen,
    RParen,
    LSquare,
    RSquare,
    LCurly,
    RCurly,
    Semicolon,
    Colon,
    Comma,
    Dot,
    QuestionMark,
    Hashtag,
    At,
    Arrow,
}

impl TokenType {
    pub fn err_str(&self) -> String {
        match self {
            Self::IntLit(_) => "integer".to_owned(),
            Self::FloatLit(_) => "float".to_owned(),
            Self::StringLit => "string".to_owned(),
            Self::Identifier => "identifier".to_owned(),
            Self::Let => "keyword `let`".to_owned(),
            Self::If => "keyword `if`".to_owned(),
            Self::Else => "keyword `else`".to_owned(),
            Self::While => "keyword `while`".to_owned(),
            Self::For => "keyword `for`".to_owned(),
            Self::In => "keyword `in`".to_owned(),
            Self::Return => "keyword `return`".to_owned(),
            Self::Extern => "keyword `extern`".to_owned(),
            Self::Export => "keyword `export`".to_owned(),
            Self::Do => "keyword `do`".to_owned(),
            Self::Then => "keyword `then`".to_owned(),
            Self::With => "keyword `with`".to_owned(),
            Self::True => "boolean `true`".to_owned(),
            Self::False => "boolean `false`".to_owned(),
            Self::Operator(op) => format!("`{}`", op.err_str()),
            Self::LParen => "`(`".to_owned(),
            Self::RParen => "`)`".to_owned(),
            Self::LSquare => "`[`".to_owned(),
            Self::RSquare => "`]`".to_owned(),
            Self::LCurly => "`{`".to_owned(),
            Self::RCurly => "`}`".to_owned(),
            Self::Semicolon => "`;`".to_owned(),
            Self::Colon => "`:`".to_owned(),
            Self::Comma => "`,`".to_owned(),
            Self::Dot => "`.`".to_owned(),
            Self::QuestionMark => "`?`".to_owned(),
            Self::Hashtag => "`#`".to_owned(),
            Self::At => "`@`".to_owned(),
            Self::Arrow => "`->`".to_owned(),
        }
    }
}

#[derive(Clone)]
pub struct Token<'tk> {
    pub ty: TokenType,
    pub lex: &'tk str,
    pub span: Span,
}

impl Token<'_> {
    pub fn err_str(&self) -> String {
        match self.ty {
            TokenType::IntLit(v) => format!("integer `{v}`"),
            TokenType::FloatLit(v) => format!("float `{v}`"),
            TokenType::StringLit => format!("string `\"{}\"`", self.lex),
            TokenType::Identifier => format!("identifier `{}`", self.lex),
            TokenType::Let
            | TokenType::If
            | TokenType::Else
            | TokenType::While
            | TokenType::For
            | TokenType::In => format!("reserved keyword `{}`", self.lex),
            _ => format!("`{}`", self.lex),
        }
    }
}

impl fmt::Debug for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.ty {
            TokenType::IntLit(v) => write!(f, "IntLit({v})"),
            TokenType::FloatLit(v) => write!(f, "FloatLit({v})"),
            TokenType::StringLit => write!(f, "StringLit(\"{}\")", self.lex),
            TokenType::Identifier => write!(f, "Identifier(`{}`)", self.lex),
            _ => write!(f, "{:?}", self.ty),
        }
    }
}
