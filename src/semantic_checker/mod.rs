pub mod types;

use crate::{
    common::{Operator, Span},
    diagnostic::Diagnostic,
    parser::{
        ast::{ASTModule, ASTNode, ASTNodeType},
        ftypes::{FrontendType, FrontendTypeType},
    },
};
use phf::phf_map;
use std::{collections::HashMap, path::PathBuf};
use types::Type;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Context {
    Root,
    FunctionDecl,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    pub ty: Type,
    pub is_initialized: bool,
    defined_at: Span,
}

static TYPES: phf::Map<&'static str, Type> = phf_map! {
    "int" => Type::Int,
    "float" => Type::Float,
    "string" => Type::String,
    "bool" => Type::Bool,
};

pub struct SemanticChecker {
    path: PathBuf,
    symbols: Vec<HashMap<String, Symbol>>,
    context: Context,
}

impl SemanticChecker {
    pub fn new(path: PathBuf) -> Self {
        Self {
            path,
            symbols: vec![HashMap::new()],
            context: Context::Root,
        }
    }

    pub fn define_symbol(&mut self, name: &str, ty: Type, is_initialized: bool, defined_at: Span) {
        self.symbols.last_mut().unwrap().insert(
            name.to_string(),
            Symbol {
                ty,
                is_initialized,
                defined_at,
            },
        );
    }

    pub fn mutate_symbol(&mut self, name: &str, ty: &Type, span: Span) -> Result<(), Diagnostic> {
        for scope in self.symbols.iter_mut().rev() {
            if let Some(symbol) = scope.get_mut(name) {
                if symbol.ty == Type::Unknown {
                    symbol.ty = ty.clone();
                    symbol.is_initialized = true;
                }
                if symbol.ty != *ty {
                    return Err(Diagnostic::new(
                        self.path.display(),
                        format!(
                            "Variable `{name}` is defined as `{}` but found `{}`",
                            &symbol.ty, &ty
                        ),
                        span,
                    )
                    .with_secondary_message(
                        Some("Variable was defined here:"),
                        symbol.defined_at.clone(),
                    ));
                }
                return Ok(());
            }
        }

        Err(Diagnostic::new(
            self.path.display(),
            format!("Couldn't find variable `{name}` in scope"),
            span,
        ))
    }

    pub fn find_symbol(&mut self, name: &str, span: Span) -> Result<&Symbol, Diagnostic> {
        self.symbols
            .iter()
            .rev()
            .find_map(|scope| scope.get(name))
            .ok_or_else(|| {
                Diagnostic::new(
                    self.path.display(),
                    format!("Couldn't find variable `{name}` in scope"),
                    span,
                )
            })
    }

    pub fn resolve_type(&self, ftype: &FrontendType) -> Result<Type, Diagnostic> {
        match &ftype.ty {
            FrontendTypeType::Identifier(ident) => {
                TYPES.get(ident).cloned().ok_or(Diagnostic::new(
                    self.path.display(),
                    format!("Cannot find type `{ident}`"),
                    ftype.span.clone(),
                ))
            }
            FrontendTypeType::Unit => Ok(Type::Unit),
            FrontendTypeType::Vector(vec) => Ok(Type::Vector(Box::new(self.resolve_type(vec)?))),
        }
    }

    fn collect_function(&mut self, node: &ASTNode) -> Result<(), Diagnostic> {
        if let ASTNodeType::FunDef {
            name,
            params,
            return_ty,
            body,
        } = &node.ty
        {
            let params_ty = params
                .iter()
                .map(|(_, ty, _)| self.resolve_type(ty))
                .collect::<Result<_, _>>()?;

            let return_ty = Box::new(match return_ty {
                Some(ty) => self.resolve_type(ty)?,
                None => Type::Unit,
            });

            self.define_symbol(name, Type::Func(params_ty, return_ty), true, node.span.clone());
        }

        Ok(())
    }

    fn collect_function_deep(&mut self, node: &ASTNode) -> Result<(), Diagnostic> {
        match &node.ty {
            ASTNodeType::FunDef {
                name,
                params,
                return_ty,
                body,
            } => {
                let params_ty = params
                    .iter()
                    .map(|(_, ty, _)| self.resolve_type(ty))
                    .collect::<Result<_, _>>()?;

                let return_ty = Box::new(match return_ty {
                    Some(ty) => self.resolve_type(ty)?,
                    None => Type::Unit,
                });

                self.define_symbol(name, Type::Func(params_ty, return_ty), true, node.span.clone());
            }
            ASTNodeType::Semi(s) => return self.collect_function_deep(s),
            _ => {}
        }

        Ok(())
    }

    pub fn validate(&mut self, ast: &mut ASTModule) -> Result<(), Vec<Diagnostic>> {
        let mut diagnostics = Vec::new();

        for node in &ast.nodes {
            if let Err(diag) = self.collect_function_deep(node) {
                diagnostics.push(diag);
            }
        }

        for node in &mut ast.nodes {
            if let Err(diag) = self.validate_node(node, false, false) {
                diagnostics.extend(diag);
            }
        }

        if !diagnostics.is_empty() {
            return Err(diagnostics);
        }

        Ok(())
    }

    pub fn validate_node(
        &mut self,
        node: &mut ASTNode,
        force_exhaustive: bool,
        recollect: bool,
    ) -> Result<Type, Vec<Diagnostic>> {
        if recollect {
            self.collect_function(node).map_err(|err| vec![err])?;
        }

        macro_rules! root_check {
            ($name:literal, $ty:ident) => {{
                if self.context == Context::Root {
                    Err(vec![Diagnostic::new(
                        self.path.display(),
                        concat!($name, " can't be a root-level item"),
                        node.span.clone(),
                    )])
                } else {
                    Ok(Type::$ty)
                }
            }};
        }

        match &mut node.ty {
            ASTNodeType::IntLit(_) => root_check!("Integer literals", Int),
            ASTNodeType::FloatLit(_) => root_check!("Float literals", Float),
            ASTNodeType::StringLit(_) => root_check!("String literals", String),
            ASTNodeType::Boolean(_) => root_check!("Booleans", Bool),
            ASTNodeType::Unit => root_check!("Units", Unit),
            ASTNodeType::Identifier(s) => {
                if self.context == Context::Root {
                    Err(vec![Diagnostic::new(
                        self.path.display(),
                        "Identifiers can't be a root-level item",
                        node.span.clone(),
                    )])
                } else {
                    self.find_symbol(s, node.span.clone())
                        .map(|sy| sy.ty.clone())
                        .map_err(|err| vec![err])
                }
            }
            ASTNodeType::Semi(stmt) => {
                self.validate_node(stmt, false, true)?;
                Ok(Type::Unit)
            }
            ASTNodeType::Block(stmts) => {
                if self.context == Context::Root {
                    Err(vec![Diagnostic::new(
                        self.path.display(),
                        "Blocks can't be a root-level item",
                        node.span.clone(),
                    )])
                } else {
                    let mut last_ty = Type::Unit;

                    for stmt in stmts.iter() {
                        self.collect_function_deep(stmt).map_err(|err| vec![err])?;
                    }

                    for stmt in stmts {
                        last_ty = self.validate_node(stmt, false, false)?;
                    }

                    Ok(last_ty)
                }
            }
            ASTNodeType::BinaryOp {
                op,
                lhs,
                rhs,
                op_tys,
            } => {
                if self.context == Context::Root {
                    Err(vec![Diagnostic::new(
                        self.path.display(),
                        "Binary operations can't be a root-level item",
                        node.span.clone(),
                    )])
                } else {
                    if [
                        Operator::Assign,
                        Operator::PlusAssign,
                        Operator::MinusAssign,
                        Operator::StarAssign,
                        Operator::SlashAssign,
                        Operator::ModuloAssign,
                    ]
                    .contains(op)
                    {
                        match &lhs.ty {
                            ASTNodeType::Identifier(s) => {
                                let rhs_ty = self.validate_node(rhs, true, true)?;
                                self.mutate_symbol(s, &rhs_ty, node.span.clone())
                                    .map_err(|err| vec![err])?;
                                let sym = self
                                    .find_symbol(s, node.span.clone())
                                    .map_err(|err| vec![err])?
                                    .clone();
                                if *op != Operator::Assign
                                    && ![Type::Int, Type::Float].contains(&sym.ty)
                                {
                                    return Err(
                                    vec![Diagnostic::new(
                                        self.path.display(),
                                        format!("Cannot use arithmetic mutation on variable of type `{}`", sym.ty),
                                        node.span.clone(),
                                    ).with_secondary_message(Some(format!("Variable `{s}` was defined here:")), sym.defined_at)]
                                );
                                }
                                let var_ty = sym.ty;
                                *op_tys = Some((var_ty, rhs_ty));
                                return Ok(Type::Unit);
                            }
                            _ => {
                                return Err(vec![Diagnostic::new(
                                    self.path.display(),
                                    "Can only mutate variables",
                                    node.span.clone(),
                                )]);
                            }
                        }
                    }

                    let lhs_ty = self.validate_node(lhs, true, true)?;
                    let rhs_ty = self.validate_node(rhs, true, true)?;

                    if let Some(output) = op.infix_output(&lhs_ty, &rhs_ty) {
                        *op_tys = Some((lhs_ty, rhs_ty));
                        Ok(output)
                    } else {
                        Err(vec![Diagnostic::new(
                            self.path.display(),
                            format!(
                                "Cannot apply `{op}` as an infix operator on types `{lhs_ty}` and `{rhs_ty}`",
                            ),
                            node.span.clone(),
                        )])
                    }
                }
            }
            ASTNodeType::UnaryOp { op, operand, op_ty } => {
                if self.context == Context::Root {
                    Err(vec![Diagnostic::new(
                        self.path.display(),
                        "Unary operations can't be a root-level item",
                        node.span.clone(),
                    )])
                } else {
                    let operand_ty = self.validate_node(operand, true, true)?;

                    if let Some(output) = op.prefix_output(&operand_ty) {
                        *op_ty = Some(operand_ty);
                        Ok(output)
                    } else {
                        Err(vec![Diagnostic::new(
                            self.path.display(),
                            format!(
                                "Cannot apply `{op}` as a unary operator on type `{operand_ty}`",
                            ),
                            node.span.clone(),
                        )])
                    }
                }
            }
            ASTNodeType::LetDecl { name, ty, init } => {
                if self.context == Context::Root {
                    Err(vec![Diagnostic::new(
                        self.path.display(),
                        "Variable declarations can't be a root-level item",
                        node.span.clone(),
                    )])
                } else {
                    let mut var_ty = if let Some(ty) = ty {
                        self.resolve_type(ty).map_err(|err| vec![err])?
                    } else {
                        Type::Unknown
                    };
                    if let Some(i) = init {
                        let init_ty = self.validate_node(i, true, true)?;
                        if var_ty == Type::Unknown {
                            var_ty = init_ty.clone();
                        }
                        if init_ty != var_ty {
                            return Err(vec![Diagnostic::new(
                                self.path.display(),
                                format!(
                                    "Variable `{name}` is declared as `{var_ty}` but initialized as `{init_ty}`",
                                ),
                                i.span.clone(),
                            )]);
                        }
                        self.define_symbol(name, var_ty, false, node.span.clone());
                    } else {
                        self.define_symbol(name, var_ty, true, node.span.clone());
                    }

                    Ok(Type::Unit)
                }
            }
            ASTNodeType::If {
                condition,
                then_body,
                else_body,
            } => {
                if self.context == Context::Root {
                    Err(vec![Diagnostic::new(
                        self.path.display(),
                        "`if` conditionals can't be a root-level item",
                        node.span.clone(),
                    )])
                } else {
                    let mut errors = Vec::new();
                    let _ = self
                        .validate_node(condition, true, true)
                        .inspect_err(|err| errors.extend(err.clone()))
                        .inspect(|ty| {
                            if *ty != Type::Bool {
                                errors.push(Diagnostic::new(
                                    self.path.display(),
                                    "Expected boolean condition in `if`",
                                    condition.span.clone(),
                                ));
                            }
                        });

                    let then_body_ty = self.validate_node(then_body, force_exhaustive, true)?;
                    if force_exhaustive {
                        if let Some(else_body) = else_body {
                            let else_body_ty =
                                self.validate_node(else_body, force_exhaustive, true)?;

                            if else_body_ty == then_body_ty {
                                Ok(then_body_ty)
                            } else {
                                errors.push(Diagnostic::new(
                                    self.path.display(),
                                    format!(
                                        "`if`'s clauses has different return types: `{then_body_ty}` and `{else_body_ty}`",
                                    ),
                                    node.span.clone(),
                                ));
                                Err(errors)
                            }
                        } else {
                            errors.push(Diagnostic::new(
                                self.path.display(),
                                format!(
                                    "Missing `else` clause that evaluates to type `{then_body_ty}`",
                                ),
                                node.span.clone(),
                            ));
                            Err(errors)
                        }
                    } else {
                        Ok(Type::Unit)
                    }
                }
            }
            ASTNodeType::While { condition, body } => {
                if self.context == Context::Root {
                    Err(vec![Diagnostic::new(
                        self.path.display(),
                        "`while` loops can't be a root-level item",
                        node.span.clone(),
                    )])
                } else {
                    let mut errors = Vec::new();
                    let cond_ty = self.validate_node(condition, true, true)?;
                    if cond_ty != Type::Bool {
                        errors.push(Diagnostic::new(
                            self.path.display(),
                            "Expected boolean condition in `while`",
                            condition.span.clone(),
                        ));
                    }

                    self.symbols.push(HashMap::new());
                    let _ = self
                        .validate_node(body, force_exhaustive, true)
                        .inspect_err(|err| errors.extend_from_slice(err.as_slice()));
                    if !errors.is_empty() {
                        return Err(errors);
                    }
                    self.symbols.pop();
                    Ok(Type::Unit)
                }
            }
            ASTNodeType::FunDef {
                name,
                params,
                return_ty,
                body,
            } => {
                let mut scope = HashMap::new();
                for (name, ty, span) in params {
                    scope.insert(
                        name.clone(),
                        Symbol {
                            ty: self.resolve_type(ty).map_err(|err| vec![err])?,
                            is_initialized: true,
                            defined_at: span.clone(),
                        },
                    );
                }
                self.symbols.push(scope);
                let previous = self.context;
                self.context = Context::FunctionDecl;
                let body_ty = self.validate_node(body, true, true)?;
                let return_ty = return_ty
                    .as_ref()
                    .map_or(Ok(Type::Unit), |ty| self.resolve_type(ty))
                    .map_err(|err| vec![err])?;
                if body_ty != return_ty {
                    return Err(vec![Diagnostic::new(
                        self.path.display(),
                        format!(
                            "Function `{name}` is declared as a function of return type `{return_ty}`, but body returns `{body_ty}`",
                        ),
                        body.span.clone(),
                    )]);
                }
                self.symbols.pop();
                self.context = previous;
                Ok(return_ty)
            }
            ASTNodeType::FunCall { callee, args } => {
                if self.context == Context::Root {
                    Err(vec![Diagnostic::new(
                        self.path.display(),
                        "Function calls can't be a root-level item",
                        node.span.clone(),
                    )])
                } else {
                    let symbol = self
                        .find_symbol(callee, node.span.clone())
                        .map_err(|err| vec![err])?
                        .clone();
                    if let Type::Func(params_ty, ty) = symbol.ty {
                        for (i, arg) in args.iter_mut().enumerate() {
                            let arg_ty = self.validate_node(arg, true, true)?;
                            if params_ty[i] != arg_ty {
                                return Err(vec![Diagnostic::new(
                                    self.path.display(),
                                    format!(
                                        "Function has argument #{i} as type `{}` but found `{arg_ty}`",
                                        params_ty[i],
                                    ),
                                    node.span.clone(),
                                )]);
                            }
                        }
                        Ok(*ty)
                    } else {
                        Err(vec![Diagnostic::new(
                            self.path.display(),
                            format!("Identifier {callee} is not a function"),
                            node.span.clone(),
                        )])
                    }
                }
            }
        }
    }
}
