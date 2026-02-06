pub mod ast;
pub mod ftypes;

use std::path::{Path, PathBuf};

use super::lexer::token::{Token, TokenType};
use crate::{common::{Operator, Span}, diagnostic::Diagnostic};
use ast::{ASTModule, ASTNode, ASTNodeType};
use ftypes::{FrontendType, FrontendTypeType};

pub struct Parser<'p> {
    path: PathBuf,
    tokens: &'p [Token<'p>],
    pos: usize,
}

impl<'p> Parser<'p> {
    pub fn new<P: AsRef<Path>>(path: P, tokens: &'p [Token]) -> Parser<'p> {
        Parser {
            path: path.as_ref().to_path_buf(),
            tokens,
            pos: 0,
        }
    }

    pub fn parse(&mut self) -> Result<ASTModule, Vec<Diagnostic>> {
        let mut module = Vec::new();
        if self.tokens.is_empty() {
            return Ok(ASTModule {
                path: self.path.clone(),
                nodes: module,
            });
        }
        let mut diagnostics = Vec::new();

        while self.tokens.get(self.pos).is_some() {
            let stmt = self.parse_stmt();
            match stmt {
                Ok(node) => module.push(node),
                Err(err) => {
                    diagnostics.push(err);
                    while let Some(token) = self.tokens.get(self.pos) {
                        // synchronize
                        if token.ty == TokenType::Semicolon
                            || token.ty == TokenType::RCurly
                            || token.ty == TokenType::RParen
                            || token.ty == TokenType::RSquare
                            || token.ty == TokenType::LCurly
                        {
                            break;
                        }
                        self.pos += 1;
                    }
                    break;
                }
            }
        }

        if !diagnostics.is_empty() {
            return Err(diagnostics);
        }
        Ok(ASTModule {
            path: self.path.clone(),
            nodes: module,
        })
    }

    fn parse_stmt(&mut self) -> Result<ASTNode, Diagnostic> {
        let mut node = self.parse_expr(0)?;

        let mut advance = false;
        if let Some(Token {
            ty: TokenType::Semicolon,
            span,
            ..
        }) = self.tokens.get(self.pos)
        {
            advance = true;
            node = ASTNode {
                span: Span::from(node.span.start..span.end),
                ty: ASTNodeType::Semi(Box::new(node)),
            };
        }

        if advance {
            self.pos += 1;
        }

        Ok(node)
    }

    fn parse_expr(&mut self, min_bp: u32) -> Result<ASTNode, Diagnostic> {
        let mut lhs = self.parse_primary()?;

        while let Some(Token {
            ty: TokenType::Operator(op),
            ..
        }) = self.tokens.get(self.pos).cloned()
        {
            if !op.is_infix() {
                break;
            }
            let (lbp, rbp) = op.precedence();
            if lbp < min_bp {
                break;
            }
            self.pos += 1;

            let rhs = self.parse_expr(rbp)?;

            lhs = ASTNode {
                span: Span::from(lhs.span.start..rhs.span.end),
                ty: ASTNodeType::BinaryOp {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    op_tys: None,
                },
            };
        }

        Ok(lhs)
    }

    fn parse_primary(&mut self) -> Result<ASTNode, Diagnostic> {
        let Some(token) = self.tokens.get(self.pos).cloned() else {
            return Err(Diagnostic::new(
                self.path.display(),
                "Expected expression, found end of input",
                self.tokens.last().unwrap().span,
            ));
        };

        match token.ty {
            TokenType::IntLit(v) => {
                self.pos += 1;
                Ok(ASTNode {
                    ty: ASTNodeType::IntLit(v),
                    span: token.span,
                })
            }
            TokenType::FloatLit(v) => {
                self.pos += 1;
                Ok(ASTNode {
                    ty: ASTNodeType::FloatLit(v),
                    span: token.span,
                })
            }
            TokenType::StringLit => {
                self.pos += 1;
                Ok(ASTNode {
                    ty: ASTNodeType::StringLit(token.lex.to_string()),
                    span: token.span,
                })
            }
            TokenType::True => {
                self.pos += 1;
                Ok(ASTNode {
                    ty: ASTNodeType::Boolean(true),
                    span: token.span,
                })
            }
            TokenType::False => {
                self.pos += 1;
                Ok(ASTNode {
                    ty: ASTNodeType::Boolean(false),
                    span: token.span,
                })
            }
            TokenType::Identifier => {
                self.pos += 1;
                if let Some(Token {
                    ty: TokenType::LParen,
                    ..
                }) = self.tokens.get(self.pos)
                {
                    self.pos += 1;
                    let mut span = token.span;
                    let mut args = Vec::new();
                    while let Some(tok) = self.tokens.get(self.pos) {
                        if tok.ty == TokenType::RParen {
                            break;
                        }
                        let node = self.parse_expr(0)?;
                        args.push(node);
                        if self.expect(TokenType::Comma).is_err() {
                            break;
                        }
                    }
                    let s = self.expect(TokenType::RParen)?;
                    span.end = s.span.end;

                    Ok(ASTNode {
                        ty: ASTNodeType::FunCall {
                            callee: token.lex.to_string(),
                            args,
                        },
                        span,
                    })
                } else {
                    Ok(ASTNode {
                        ty: ASTNodeType::Identifier(token.lex.to_string()),
                        span: token.span,
                    })
                }
            }
            TokenType::Operator(op) if op.is_prefix() => {
                self.pos += 1;
                let operand = self.parse_primary()?;
                Ok(ASTNode {
                    span: Span::from(token.span.start..operand.span.end),
                    ty: ASTNodeType::UnaryOp {
                        op,
                        operand: Box::new(operand),
                        op_ty: None,
                    },
                })
            }
            TokenType::LParen => {
                self.pos += 1;
                if let Some(Token {
                    ty: TokenType::RParen,
                    span,
                    ..
                }) = self.tokens.get(self.pos)
                {
                    Ok(ASTNode {
                        span: Span::from(token.span.start..span.end),
                        ty: ASTNodeType::Unit,
                    })
                } else {
                    Ok(self.parse_expr(0)?)
                }
            }
            TokenType::LCurly => self.parse_block(),
            TokenType::Let => self.parse_let(),
            TokenType::If => self.parse_if(),
            TokenType::While => self.parse_while(),
            _ => Err(Diagnostic::new(
                self.path.display(),
                format!("Expected expression, found {}", token.err_str()),
                token.span,
            )),
        }
    }

    fn parse_block(&mut self) -> Result<ASTNode, Diagnostic> {
        let mut stmt_span = self.tokens.get(self.pos).unwrap().span;
        self.pos += 1;

        let mut stmts = Vec::new();
        while let Some(token) = self.tokens.get(self.pos) {
            if token.ty == TokenType::RCurly {
                break;
            }

            let stmt = self.parse_stmt()?;
            stmts.push(stmt);
        }

        stmt_span.end = self.expect(TokenType::RCurly)?.span.end;

        Ok(ASTNode {
            ty: ASTNodeType::Block(stmts),
            span: stmt_span,
        })
    }

    fn parse_let(&mut self) -> Result<ASTNode, Diagnostic> {
        let mut stmt_span = self.tokens.get(self.pos).unwrap().span;
        self.pos += 1;

        let ident = self.expect(TokenType::Identifier)?;
        let name = ident.lex.to_string();
        stmt_span.end = ident.span.end;

        if self.expect(TokenType::LParen).is_ok() {
            let mut params = Vec::new();
            while let Some(tok) = self.tokens.get(self.pos) {
                if tok.ty == TokenType::RParen {
                    break;
                }
                let ident = self.expect(TokenType::Identifier)?;
                let mut param_span = ident.span;
                let name = ident.lex.to_string();
                self.expect(TokenType::Colon)?;
                let ty = self.parse_type()?;
                param_span.end = ty.span.end;
                params.push((name, ty, param_span));
                if self.expect(TokenType::Comma).is_err() {
                    break;
                }
            }
            self.expect(TokenType::RParen)?;
            let return_ty = if self.expect(TokenType::Colon).is_ok() {
                Some(self.parse_type()?)
            } else {
                None
            };
            self.expect(TokenType::Operator(Operator::Assign))?;
            let body = self.parse_expr(0)?;
            stmt_span.end = body.span.end;

            return Ok(ASTNode {
                ty: ASTNodeType::FunDef {
                    name,
                    params,
                    return_ty,
                    body: Box::new(body),
                },
                span: stmt_span,
            });
        }

        let ty = if self.expect(TokenType::Colon).is_ok() {
            let expr = self.parse_type()?;
            stmt_span.end = expr.span.end;
            Some(expr)
        } else {
            None
        };

        let init = if self.expect(TokenType::Operator(Operator::Assign)).is_ok() {
            let expr = self.parse_expr(0)?;
            stmt_span.end = expr.span.end;
            Some(expr)
        } else {
            None
        };

        Ok(ASTNode {
            ty: ASTNodeType::LetDecl {
                name,
                ty,
                init: init.map(Box::new),
            },
            span: stmt_span,
        })
    }

    fn parse_if(&mut self) -> Result<ASTNode, Diagnostic> {
        let mut stmt_span = self.tokens.get(self.pos).unwrap().span;
        self.pos += 1;

        let condition = self.parse_expr(0)?;
        self.expect(TokenType::Then)?;

        let then_body = self.parse_expr(0)?;
        stmt_span.end = then_body.span.end;

        let else_body = if self.expect(TokenType::Else).is_ok() {
            let expr = self.parse_expr(0)?;
            stmt_span.end = expr.span.end;
            Some(expr)
        } else {
            None
        };

        Ok(ASTNode {
            ty: ASTNodeType::If {
                condition: Box::new(condition),
                then_body: Box::new(then_body),
                else_body: else_body.map(Box::new),
            },
            span: stmt_span,
        })
    }

    fn parse_while(&mut self) -> Result<ASTNode, Diagnostic> {
        let mut stmt_span = self.tokens.get(self.pos).unwrap().span;
        self.pos += 1;

        let condition = self.parse_expr(0)?;
        self.expect(TokenType::Do)?;

        let body = self.parse_expr(0)?;
        stmt_span.end = body.span.end;

        Ok(ASTNode {
            ty: ASTNodeType::While {
                condition: Box::new(condition),
                body: Box::new(body),
            },
            span: stmt_span,
        })
    }

    fn parse_type(&mut self) -> Result<FrontendType, Diagnostic> {
        let Some(token) = self.tokens.get(self.pos).cloned() else {
            return Err(Diagnostic::new(
                self.path.display(),
                "Expected type, found end of input",
                self.tokens.last().unwrap().span,
            ));
        };

        match token.ty {
            TokenType::Identifier => {
                self.pos += 1;
                Ok(FrontendType {
                    ty: FrontendTypeType::Identifier(token.lex.to_string()),
                    span: token.span,
                })
            }
            TokenType::LParen => {
                self.pos += 1;
                if let Some(Token {
                    ty: TokenType::RParen,
                    span,
                    ..
                }) = self.tokens.get(self.pos)
                {
                    Ok(FrontendType {
                        ty: FrontendTypeType::Unit,
                        span: Span::from(token.span.start..span.end),
                    })
                } else {
                    Ok(self.parse_type()?)
                }
            }
            TokenType::LSquare => {
                self.pos += 1;
                let inner_ty = self.parse_type()?;
                self.expect(TokenType::RSquare)?;
                Ok(FrontendType {
                    ty: FrontendTypeType::Vector(Box::new(inner_ty)),
                    span: token.span,
                })
            }
            _ => Err(Diagnostic::new(
                self.path.display(),
                format!("Expected type, found {}", token.err_str()),
                self.tokens.last().unwrap().span,
            )),
        }
    }

    fn expect(&mut self, expected: TokenType) -> Result<Token<'p>, Diagnostic> {
        if let Some(token) = self.tokens.get(self.pos).cloned() {
            if token.ty == expected {
                self.pos += 1;
                Ok(token)
            } else {
                Err(Diagnostic::new(
                    self.path.display(),
                    format!("Expected {}, found {}", expected.err_str(), token.err_str()),
                    token.span,
                ))
            }
        } else {
            Err(Diagnostic::new(
                self.path.display(),
                format!("Expected {}, found end of input", expected.err_str()),
                self.tokens.last().unwrap().span,
            ))
        }
    }
}
