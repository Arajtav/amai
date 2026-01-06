pub mod types;

use std::collections::HashMap;
use crate::{Span, diagnostic::Diagnostic, parser::{ast::*, pattern::*, ftypes::*}};
use types::*;

#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    pub ty: Type,
    pub mutability: bool,
    pub is_unitialized: bool,
    defined_at: Span,
}

pub struct SemanticChecker<'t> {
    ast: &'t AmaiASTModule,
    symbols: Vec<HashMap<String, Symbol>>,
    type_registry: HashMap<String, Type>,
}

impl<'t> SemanticChecker<'t> {
    pub fn new(ast: &'t AmaiASTModule) -> SemanticChecker<'t> {
        let mut t = SemanticChecker {
            ast, symbols: vec![HashMap::new()],
            type_registry: HashMap::new()
        };

        t.type_registry.insert("int".to_string(), Type::Int);
        t.type_registry.insert("float".to_string(), Type::Float);
        t.type_registry.insert("string".to_string(), Type::String);
        t.type_registry.insert("bool".to_string(), Type::Bool);

        t
    }

    pub fn define_symbol(
        &mut self,
        name: &str,
        ty: Type,
        mutability: bool,
        is_unitialized: bool,
        defined_at: Span,
    ) {
        self.symbols.last_mut().unwrap().insert(name.to_string(), Symbol { ty, mutability, is_unitialized, defined_at });
    }

    pub fn mutate_symbol(
        &mut self,
        name: &str,
        ty: Type,
        span: Span,
    ) -> Result<(), Diagnostic> {
        for scope in self.symbols.iter_mut().rev() {
            if let Some(symbol) = scope.get_mut(name) {
                if !symbol.mutability && !symbol.is_unitialized {
                    return Err(
                        Diagnostic::new(
                            &self.ast.path,
                            format!("Variable `{name}` is immutable"),
                            span,
                        ).with_secondary_message(Some("Variable was defined here:"), symbol.defined_at.clone())
                    );
                }
                if symbol.ty != ty && symbol.ty != Type::Unknown {
                    return Err(
                        Diagnostic::new(
                            &self.ast.path,
                            format!("Variable `{name}` is defined as `{}` but found `{}`", symbol.ty.display(), ty.display()),
                            span,
                        ).with_secondary_message(Some("Variable was defined here:"), symbol.defined_at.clone())
                    );
                }

                if symbol.ty == Type::Unknown {
                    symbol.ty = ty;
                    symbol.is_unitialized = false;
                }
                return Ok(());
            }
        }

        return Err(
            Diagnostic::new(
                &self.ast.path,
                format!("Couldn't find variable `{name}` in scope"),
                span,
            )
        );
    }

    pub fn find_symbol(&mut self, name: &str, span: Span) -> Result<&Symbol, Diagnostic>  {
        for scope in self.symbols.iter().rev() {
            if let Some(symbol) = scope.get(name) {
                return Ok(symbol);
            }
        }

        return Err(
            Diagnostic::new(
                &self.ast.path,
                format!("Couldn't find variable `{name}` in scope"),
                span,
            )
        );
    }

    pub fn resolve_type(&self, ftype: &FrontendType) -> Result<Type, Diagnostic> {
        match ftype.kind {
            FrontendTypeKind::Identifier(ident) => self.type_registry.get(&ident).cloned()
                .ok_or(Diagnostic::new(&self.ast.path, format!("Cannot identifier type `{}`", ident), ftype.span)),
            FrontendTypeKind::Unit => Ok(Type::Unit),
            FrontendTypeKind::Vector(vec) => Ok(Type::Vector(Box::new(self.resolve_type(&vec)?))),
            FrontendTypeKind::Tuple(tup) => {
                let mut tup_items = Vec::new();
            }
        }
    }

    pub fn validate(&mut self) -> Result<(), Vec<Diagnostic>> {
        let nodes = self.ast.nodes.clone();
        let mut diagnostics = Vec::new();

        for node in nodes {
            if let Err(diag) = self.validate_node(&node) {
                diagnostics.push(diag);
            }
        }

        if !diagnostics.is_empty() {
            return Err(diagnostics);
        }

        Ok(())
    }

    pub fn validate_node(&mut self, node: &AmaiASTNode) -> Result<Type, Diagnostic> {
        match &node.kind {
            AmaiASTNodeKind::IntLit(_) => Ok(Type::Int),
            AmaiASTNodeKind::FloatLit(_) => Ok(Type::Float),
            AmaiASTNodeKind::Boolean(_) => Ok(Type::Bool),
            AmaiASTNodeKind::Identifier(s) =>
                self
                    .find_symbol(s, node.span.clone())
                    .map(|sy| sy.ty.clone()),
            AmaiASTNodeKind::Semi(stmt) => {
                self.validate_node(stmt)?;
                Ok(Type::Unit)
            },
            AmaiASTNodeKind::Tuple(items) => {
                let mut item_tys = Vec::new();

                for i in items {
                    item_tys.push(self.validate_node(i)?);
                }

                Ok(Type::Tuple(item_tys))
            },
            AmaiASTNodeKind::Block(stmts) => {
                let mut last_ty = Type::Unit;

                for stmt in stmts {
                    last_ty = self.validate_node(stmt)?;
                }

                Ok(last_ty)
            },
            AmaiASTNodeKind::Unit => Ok(Type::Unit),
            AmaiASTNodeKind::BinaryOp { op, lhs, rhs } => {
                match &lhs.kind {
                    AmaiASTNodeKind::Identifier(s) => {
                        let rhs_ty = self.validate_node(rhs)?;
                        self.mutate_symbol(s, rhs_ty, node.span.clone())?;
                        return Ok(Type::Unit);
                    },
                    _ => {},
                }
                
                let lhs_ty = self.validate_node(lhs)?;
                let rhs_ty = self.validate_node(rhs)?;

                if let Some(output) = op.infix_output(&lhs_ty, &rhs_ty) {
                    Ok(output)
                } else {
                    Err(
                        Diagnostic::new(
                            &self.ast.path,
                            format!(
                                "Cannot apply `{}` as an infix operator on types `{}` and `{}`",
                                op.err_str(),
                                lhs_ty.display(),
                                rhs_ty.display()
                            ),
                            node.span.clone(),
                        )
                    )
                }
            },
            AmaiASTNodeKind::UnaryOp { op, operand} => {
                let operand_ty = self.validate_node(operand)?;

                if let Some(output) = op.prefix_output(&operand_ty) {
                    Ok(output)
                } else {
                    Err(
                        Diagnostic::new(
                            &self.ast.path,
                            format!(
                                "Cannot apply `{}` as a unary operator on type `{}`",
                                op.err_str(),
                                operand_ty.display()
                            ),
                            node.span.clone(),
                        )
                    )
                }
            },
            AmaiASTNodeKind::LetDecl { pat, ty, init } => {
                match pat.kind {
                    PatternKind::Identifier(s) => {
                        let var_ty = if let Some(ty) = ty {
                            ty
                        } else {
                        };
                    },
                    PatternKind::Literal(s) => {},
                }
            }
            _ => todo!("{:#?}", node.kind),
        }
    }
}