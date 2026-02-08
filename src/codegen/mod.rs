use crate::common::{Operator, Span};
use crate::parser::ast::{ASTModule, ASTNode, ASTNodeType};
use crate::semantic_checker::types::Type;
use crate::vm::inst::Opcode;
use std::collections::HashMap;

pub mod function_builder;
pub mod value;
use function_builder::FunctionBuilder;
use value::ValueBuilder;

macro_rules! set_arg {
    ($inst:ident, $ty:ty, $pos:expr, $val:expr) => {{
        const LEN: usize = std::mem::size_of::<$ty>() * 8;
        const MASK: u32 = if LEN == 32 {
            u32::MAX
        } else {
            (1u32 << LEN) - 1
        };
        $inst &= !(MASK << $pos);
        $inst |= (u32::from($val) & MASK) << $pos;
        $inst
    }};
}

macro_rules! encode {
    ($variant:ident) => {{
        Opcode::$variant as u32
    }};
    ($variant:ident, [$($ty:ty : $val:expr),+ $(,)?]) => {
        #[allow(unused_assignments)]
        {
            let mut inst = Opcode::$variant as u32;
            let mut shift = 8;

            $(
                inst = set_arg!(inst, $ty, shift, $val);
                shift += std::mem::size_of::<$ty>() * 8;
            )+

            inst
        }
    };
}

pub struct ASTCompiler {
    functions: Vec<FunctionBuilder>,
    pub constants: Vec<ValueBuilder>,
    current_function: usize,
}

impl ASTCompiler {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
            constants: Vec::new(),
            current_function: 0,
        }
    }

    fn add_constant(&mut self, c: ValueBuilder) -> u16 {
        u16::try_from(
            self.constants
                .iter()
                .position(|s| s == &c)
                .unwrap_or_else(|| {
                    self.constants.push(c);
                    self.constants.len() - 1
                }),
        )
        .unwrap()
    }

    // TODO: Panics if main is not defined.
    pub fn compile(&mut self, ast: &ASTModule) -> (usize, Vec<FunctionBuilder>) {
        for node in &ast.nodes {
            self.collect_functions_sig(node);
        }
        for node in &ast.nodes {
            self.compile_node(node, &mut Vec::new(), 0);
        }
        let main_id = self.get_func("@main_0");
        (main_id, self.functions.clone())
    }

    fn collect_functions_body(&mut self, node: &ASTNode) {
        if let ASTNodeType::FunDef {
            name, params, body, ..
        } = &node.ty
        {
            let scope = self
                .functions
                .last_mut()
                .map_or(0, |func| func.current_scope_id);
            self.current_function = self.get_func(&format!("@{name}_{scope}"));
            self.functions
                .iter_mut()
                .find(|f| f.name == format!("@{name}_{scope}"))
                .unwrap()
                .scope
                .last_mut()
                .unwrap()
                .0
                .insert(format!("@{name}_{scope}"), 3);
            let mut output_buf = Vec::new();
            for (i, (name, _, _)) in params.iter().enumerate() {
                let func = self.functions.get_mut(self.current_function).unwrap();
                let (scope, next_available, sc_id) = func.scope.last_mut().unwrap();
                let reg = *next_available;
                scope.insert(format!("{name}_{sc_id}"), reg);
                *next_available += 1;

                let i = u16::try_from(i).expect("Function has too many params");
                output_buf.push((encode!(CArg, [u8:reg, u16:i]), Span::empty()));
            }
            self.compile_node(body, &mut output_buf, 0);
            output_buf.push((encode!(PArg), Span::empty()));
            output_buf.push((encode!(Retn), Span::empty()));
            self.functions
                .iter_mut()
                .find(|f| f.name == format!("@{name}_{scope}"))
                .unwrap()
                .body = output_buf;
        }
    }

    fn get_func(&self, callee: &str) -> usize {
        self.functions
            .iter()
            .position(|f| f.name == callee)
            .unwrap()
    }

    fn collect_functions_sig(&mut self, node: &ASTNode) {
        match &node.ty {
            ASTNodeType::FunDef { name, .. } => {
                let mut registers: [ValueBuilder; 64] = std::array::from_fn(|_| ValueBuilder::Unit);
                registers[3] = ValueBuilder::Function(self.current_function);
                let scope = self
                    .functions
                    .last_mut()
                    .map_or(0, |func| func.current_scope_id);
                self.functions
                    .push(FunctionBuilder::new(&format!("@{name}_{scope}"), registers));
                self.functions
                    .last_mut()
                    .unwrap()
                    .scope
                    .last_mut()
                    .unwrap()
                    .0
                    .insert(format!("@{name}_{scope}"), 3);
            }
            ASTNodeType::Semi(s) => self.collect_functions_sig(s),
            _ => {}
        }
    }

    fn compile_node(&mut self, node: &ASTNode, output_buf: &mut Vec<(u32, Span)>, dest: u8) {
        self.collect_functions_body(node);
        match &node.ty {
            ASTNodeType::IntLit(n) => {
                output_buf.push((
                    encode!(Load, [
                        u8:dest,
                        u16:self.add_constant(ValueBuilder::Int(*n))
                    ]),
                    node.span,
                ));
            }
            ASTNodeType::FloatLit(n) => {
                output_buf.push((
                    encode!(Load, [
                        u8:dest,
                        u16:self.add_constant(ValueBuilder::Float(*n))
                    ]),
                    node.span,
                ));
            }
            ASTNodeType::StringLit(n) => {
                output_buf.push((
                    encode!(Load, [
                        u8:dest,
                        u16:self.add_constant(ValueBuilder::String(n.as_bytes().to_owned()))
                    ]),
                    node.span,
                ));
            }
            ASTNodeType::Boolean(n) => {
                output_buf.push((
                    encode!(Load, [
                        u8:dest,
                        u16:self.add_constant(ValueBuilder::Bool(*n))
                    ]),
                    node.span,
                ));
            }
            ASTNodeType::Identifier(n) => output_buf.push((
                encode!(Move, [
                    u8:dest,
                    u8:self
                        .functions
                        .get_mut(self.current_function)
                        .unwrap()
                        .get_var(n),
                ]),
                node.span,
            )),
            ASTNodeType::Semi(stmt) => self.compile_node(stmt, output_buf, 0),
            ASTNodeType::Unit => {
                output_buf.push((
                    encode!(Load, [
                        u8:dest,
                        u16:self.add_constant(ValueBuilder::Unit)
                    ]),
                    node.span,
                ));
            }
            ASTNodeType::Block(stmts) => {
                if stmts.is_empty() {
                    output_buf.push((
                        encode!(Load, [
                            u8:dest,
                            u16:self.add_constant(ValueBuilder::Unit)
                        ]),
                        node.span,
                    ));
                    return;
                }

                let func = self.functions.get_mut(self.current_function).unwrap();
                let previous_scope_id = func.current_scope_id;
                let new_scope_id = *func.taken_scope_ids.iter().max().unwrap() + 1;
                func.current_scope_id = new_scope_id;
                func.taken_scope_ids.push(new_scope_id);
                func.scope.push((HashMap::new(), 3, func.current_scope_id));

                if stmts.len() == 1 {
                    self.compile_node(&stmts[0], output_buf, dest);
                } else {
                    for node in &stmts[0..stmts.len() - 1] {
                        self.compile_node(node, output_buf, 0);
                    }
                    self.compile_node(&stmts[stmts.len() - 1], output_buf, dest);
                }

                let func = self.functions.get_mut(self.current_function).unwrap();
                func.scope.pop();
                func.current_scope_id = previous_scope_id;
            }
            ASTNodeType::BinaryOp {
                op,
                lhs,
                rhs,
                op_tys,
            } => {
                if let ASTNodeType::Identifier(s) = &lhs.ty {
                    let func = self.functions.get_mut(self.current_function).unwrap();
                    let reg = func.get_var(s);

                    match *op {
                        Operator::Assign => {
                            self.compile_node(rhs, output_buf, 0);
                            output_buf.push((encode!(Move, [u8:reg]), node.span));
                            output_buf.push((
                                encode!(Load, [
                                    u8:dest,
                                    u16:self.add_constant(ValueBuilder::Unit)
                                ]),
                                node.span,
                            ));
                            return;
                        }
                        Operator::PlusAssign => {
                            self.compile_node(rhs, output_buf, 1);
                            match op_tys.as_ref().unwrap().0 {
                                Type::Int => output_buf.push((
                                    encode!(IAdd, [
                                        u8:reg,
                                        u8:reg,
                                        u8:1u32
                                    ]),
                                    node.span,
                                )),
                                Type::Float => output_buf.push((
                                    encode!(FAdd, [
                                        u8:reg,
                                        u8:reg,
                                        u8:1u32
                                    ]),
                                    node.span,
                                )),
                                _ => unreachable!(),
                            }

                            output_buf.push((
                                encode!(Load, [
                                    u8:dest,
                                    u16:self.add_constant(ValueBuilder::Unit)
                                ]),
                                node.span,
                            ));

                            return;
                        }
                        Operator::MinusAssign => {
                            self.compile_node(rhs, output_buf, 1);
                            match op_tys.as_ref().unwrap().0 {
                                Type::Int => output_buf.push((
                                    encode!(ISub, [
                                        u8:reg,
                                        u8:reg,
                                        u8:1u32
                                    ]),
                                    node.span,
                                )),
                                Type::Float => output_buf.push((
                                    encode!(FSub, [
                                        u8:reg,
                                        u8:reg,
                                        u8:1u32
                                    ]),
                                    node.span,
                                )),
                                _ => unreachable!(),
                            }

                            output_buf.push((
                                encode!(Load, [
                                    u8:dest,
                                    u16:self.add_constant(ValueBuilder::Unit)
                                ]),
                                node.span,
                            ));
                            return;
                        }
                        Operator::StarAssign => {
                            self.compile_node(rhs, output_buf, 1);
                            match op_tys.as_ref().unwrap().0 {
                                Type::Int => output_buf.push((
                                    encode!(IMul, [
                                        u8:reg,
                                        u8:reg,
                                        u8:1u32
                                    ]),
                                    node.span,
                                )),
                                Type::Float => output_buf.push((
                                    encode!(FMul, [
                                        u8:reg,
                                        u8:reg,
                                        u8:1u32
                                    ]),
                                    node.span,
                                )),
                                _ => unreachable!(),
                            }

                            output_buf.push((
                                encode!(Load, [
                                    u8:dest,
                                    u16:self.add_constant(ValueBuilder::Unit)
                                ]),
                                node.span,
                            ));
                            return;
                        }
                        Operator::SlashAssign => {
                            self.compile_node(rhs, output_buf, 1);
                            match op_tys.as_ref().unwrap().0 {
                                Type::Int => output_buf.push((
                                    encode!(IDiv, [
                                        u8:reg,
                                        u8:reg,
                                        u8:1u32
                                    ]),
                                    node.span,
                                )),
                                Type::Float => output_buf.push((
                                    encode!(FDiv, [
                                        u8:reg,
                                        u8:reg,
                                        u8:1u32
                                    ]),
                                    node.span,
                                )),
                                _ => unreachable!(),
                            }

                            output_buf.push((
                                encode!(Load, [
                                    u8:dest,
                                    u16:self.add_constant(ValueBuilder::Unit)
                                ]),
                                node.span,
                            ));
                            return;
                        }
                        Operator::ModuloAssign => {
                            self.compile_node(rhs, output_buf, 1);
                            match op_tys.as_ref().unwrap().0 {
                                Type::Int => output_buf.push((
                                    encode!(IRem, [
                                        u8:reg,
                                        u8:reg,
                                        u8:1u32
                                    ]),
                                    node.span,
                                )),
                                Type::Float => output_buf.push((
                                    encode!(FRem, [
                                        u8:reg,
                                        u8:reg,
                                        u8:1u32,
                                    ]),
                                    node.span,
                                )),
                                _ => unreachable!(),
                            }

                            output_buf.push((
                                encode!(Load, [
                                    u8:dest,
                                    u16:self.add_constant(ValueBuilder::Unit),
                                ]),
                                node.span,
                            ));
                            return;
                        }
                        _ => {}
                    }
                }

                self.compile_node(lhs, output_buf, 1);
                self.compile_node(rhs, output_buf, 2);

                match op {
                    Operator::Plus => match op_tys.as_ref().unwrap().0 {
                        Type::Int => output_buf.push((
                            encode!(IAdd, [
                                u8:dest,
                                u8:1u32,
                                u8:2u32,
                            ]),
                            node.span,
                        )),
                        Type::Float => output_buf.push((
                            encode!(FAdd, [
                                u8:dest,
                                u8:1u32,
                                u8:2u32,
                            ]),
                            node.span,
                        )),
                        _ => unreachable!(),
                    },
                    Operator::Minus => match op_tys.as_ref().unwrap().0 {
                        Type::Int => output_buf.push((
                            encode!(ISub, [
                                u8:dest,
                                u8:1u32,
                                u8:2u32,
                            ]),
                            node.span,
                        )),
                        Type::Float => output_buf.push((
                            encode!(FSub, [
                                u8:dest,
                                u8:1u32,
                                u8:2u32,
                            ]),
                            node.span,
                        )),
                        _ => unreachable!(),
                    },
                    Operator::Star => match op_tys.as_ref().unwrap().0 {
                        Type::Int => output_buf.push((
                            encode!(IMul, [
                                u8:dest,
                                u8:1u32,
                                u8:2u32,
                            ]),
                            node.span,
                        )),
                        Type::Float => output_buf.push((
                            encode!(FMul, [
                                u8:dest,
                                u8:1u32,
                                u8:2u32,
                            ]),
                            node.span,
                        )),
                        _ => unreachable!(),
                    },
                    Operator::Slash => match op_tys.as_ref().unwrap().0 {
                        Type::Int => output_buf.push((
                            encode!(IDiv, [
                                u8:dest,
                                u8:1u32,
                                u8:2u32,
                            ]),
                            node.span,
                        )),
                        Type::Float => output_buf.push((
                            encode!(FDiv, [
                                u8:dest,
                                u8:1u32,
                                u8:2u32,
                            ]),
                            node.span,
                        )),
                        _ => unreachable!(),
                    },
                    Operator::Modulo => match op_tys.as_ref().unwrap().0 {
                        Type::Int => output_buf.push((
                            encode!(IRem, [
                                u8:dest,
                                u8:1u32,
                                u8:2u32,
                            ]),
                            node.span,
                        )),
                        Type::Float => output_buf.push((
                            encode!(FRem, [
                                u8:dest,
                                u8:1u32,
                                u8:2u32,
                            ]),
                            node.span,
                        )),
                        _ => unreachable!(),
                    },
                    Operator::Eq => match op_tys.as_ref().unwrap().0 {
                        Type::String => output_buf.push((
                            encode!(SCEq, [
                                u8:dest,
                                u8:1u32,
                                u8:2u32,
                            ]),
                            node.span,
                        )),
                        _ => output_buf.push((
                            encode!(CMEq, [
                                u8:dest,
                                u8:1u32,
                                u8:2u32,
                            ]),
                            node.span,
                        )),
                    },
                    Operator::Ne => match op_tys.as_ref().unwrap().0 {
                        Type::String => output_buf.push((
                            encode!(SCNE, [
                                u8:dest,
                                u8:1u32,
                                u8:2u32,
                            ]),
                            node.span,
                        )),
                        _ => output_buf.push((
                            encode!(CMNE, [
                                u8:dest,
                                u8:1u32,
                                u8:2u32,
                            ]),
                            node.span,
                        )),
                    },
                    Operator::Gt => match op_tys.as_ref().unwrap().0 {
                        Type::Int => output_buf.push((
                            encode!(ICGT, [
                                u8:dest,
                                u8:1u32,
                                u8:2u32,
                            ]),
                            node.span,
                        )),
                        Type::Float => output_buf.push((
                            encode!(FCGT, [
                                u8:dest,
                                u8:1u32,
                                u8:2u32,
                            ]),
                            node.span,
                        )),
                        _ => unreachable!(),
                    },
                    Operator::Lt => match op_tys.as_ref().unwrap().0 {
                        Type::Int => output_buf.push((
                            encode!(ICLT, [
                                u8:dest,
                                u8:1u32,
                                u8:2u32,
                            ]),
                            node.span,
                        )),
                        Type::Float => output_buf.push((
                            encode!(FCLT, [
                                u8:dest,
                                u8:1u32,
                                u8:2u32,
                            ]),
                            node.span,
                        )),
                        _ => unreachable!(),
                    },
                    Operator::Ge => match op_tys.as_ref().unwrap().0 {
                        Type::Int => output_buf.push((
                            encode!(ICGE, [
                                u8:dest,
                                u8:1u32,
                                u8:2u32,
                            ]),
                            node.span,
                        )),
                        Type::Float => output_buf.push((
                            encode!(FCGE, [
                                u8:dest,
                                u8:1u32,
                                u8:2u32,
                            ]),
                            node.span,
                        )),
                        _ => unreachable!(),
                    },
                    Operator::Le => match op_tys.as_ref().unwrap().0 {
                        Type::Int => output_buf.push((
                            encode!(ICLE, [
                                u8:dest,
                                u8:1u32,
                                u8:2u32,
                            ]),
                            node.span,
                        )),
                        Type::Float => output_buf.push((
                            encode!(FCLE, [
                                u8:dest,
                                u8:1u32,
                                u8:2u32,
                            ]),
                            node.span,
                        )),
                        _ => unreachable!(),
                    },
                    Operator::Concat => output_buf.push((
                        encode!(SCon, [
                            u8:dest,
                            u8:1u32,
                            u8:2u32,
                        ]),
                        node.span,
                    )),
                    Operator::LogOr => output_buf.push((
                        encode!(LOr, [
                            u8:dest,
                            u8:1u32,
                            u8:2u32,
                        ]),
                        node.span,
                    )),
                    Operator::LogAnd => output_buf.push((
                        encode!(LAnd, [
                            u8:dest,
                            u8:1u32,
                            u8:2u32,
                        ]),
                        node.span,
                    )),
                    Operator::Pipe => output_buf.push((
                        encode!(BOr, [
                            u8:dest,
                            u8:1u32,
                            u8:2u32,
                        ]),
                        node.span,
                    )),
                    Operator::Ampersand => output_buf.push((
                        encode!(BAnd, [
                            u8:dest,
                            u8:1u32,
                            u8:2u32,
                        ]),
                        node.span,
                    )),
                    Operator::Caret => output_buf.push((
                        encode!(BXor, [
                            u8:dest,
                            u8:1u32,
                            u8:2u32,
                        ]),
                        node.span,
                    )),
                    Operator::Lsh => output_buf.push((
                        encode!(LShf, [
                            u8:dest,
                            u8:1u32,
                            u8:2u32,
                        ]),
                        node.span,
                    )),
                    Operator::Rsh => output_buf.push((
                        encode!(RShf, [
                            u8:dest,
                            u8:1u32,
                            u8:2u32,
                        ]),
                        node.span,
                    )),
                    _ => todo!("binary op: {op:?}"),
                }
            }
            ASTNodeType::UnaryOp { op, operand, op_ty } => {
                self.compile_node(operand, output_buf, 1);

                match *op {
                    Operator::Plus => output_buf.push((
                        encode!(Move, [
                            u8:dest,
                            u8:1u32,
                        ]),
                        node.span,
                    )),
                    Operator::Minus => match op_ty.as_ref().unwrap() {
                        Type::Int => output_buf.push((
                            encode!(INeg, [
                                u8:dest,
                                u8:1u32
                            ]),
                            node.span,
                        )),
                        Type::Float => output_buf.push((
                            encode!(FNeg, [
                                u8:dest,
                                u8:1u32
                            ]),
                            node.span,
                        )),
                        _ => unreachable!(),
                    },
                    Operator::Tilde => output_buf.push((
                        encode!(BNot, [
                            u8:dest,
                            u8:1u32
                        ]),
                        node.span,
                    )),
                    Operator::Bang => output_buf.push((
                        encode!(LNot, [
                            u8:dest,
                            u8:1u32
                        ]),
                        node.span,
                    )),
                    _ => unreachable!(),
                }
            }
            ASTNodeType::LetDecl { name, init, .. } => {
                let func = self.functions.get_mut(self.current_function).unwrap();
                let (scope, next_available, sc_id) = func.scope.last_mut().unwrap();
                let reg = *next_available;
                scope.insert(format!("{name}_{sc_id}"), reg);
                *next_available += 1;
                if let Some(i) = init {
                    self.compile_node(i, output_buf, 0);
                    output_buf.push((
                        encode!(Move, [
                            u8:reg,
                        ]),
                        node.span,
                    ));
                }

                output_buf.push((
                    encode!(Load, [
                        u8:dest,
                        u16:self.add_constant(ValueBuilder::Unit)
                    ]),
                    node.span,
                ));
            }
            ASTNodeType::If {
                condition,
                then_body,
                else_body,
            } => {
                self.compile_node(condition, output_buf, 0);

                let func = self.functions.get_mut(self.current_function).unwrap();

                let previous_scope_id = func.current_scope_id;
                let new_scope_id = *func.taken_scope_ids.iter().max().unwrap() + 1;
                func.current_scope_id = new_scope_id;
                func.taken_scope_ids.push(new_scope_id);

                let mut compiled_then = Vec::new();
                self.compile_node(then_body, &mut compiled_then, dest);

                let mut compile = None;
                {
                    let func = self.functions.get_mut(self.current_function).unwrap();
                    func.current_scope_id = previous_scope_id;

                    output_buf.push((
                        encode!(JIFl, [
                            u32:u32::try_from(compiled_then.len()).unwrap() + 1
                        ]),
                        node.span,
                    ));
                    output_buf.extend(compiled_then);
                    if else_body.is_some() {
                        let previous_scope_id = func.current_scope_id;
                        let new_scope_id = *func.taken_scope_ids.iter().max().unwrap() + 1;
                        func.current_scope_id = new_scope_id;
                        func.taken_scope_ids.push(new_scope_id);
                        compile = else_body.clone().map(|x| (x, previous_scope_id));
                    }
                }
                if let Some((else_body, previous_scope_id)) = compile {
                    let mut compiled_else = Vec::new();
                    self.compile_node(&else_body, &mut compiled_else, dest);
                    output_buf.extend(compiled_else);
                    let func = self.functions.get_mut(self.current_function).unwrap();
                    func.current_scope_id = previous_scope_id;
                }
            }
            ASTNodeType::While { condition, body } => {
                let previous_scope_id: usize;
                let mut compiled_body: Vec<(u32, Span)>;
                {
                    let func = self.functions.get_mut(self.current_function).unwrap();

                    previous_scope_id = func.current_scope_id;
                    let new_scope_id = *func.taken_scope_ids.iter().max().unwrap() + 1;
                    func.current_scope_id = new_scope_id;
                    func.taken_scope_ids.push(new_scope_id);
                    compiled_body = Vec::new();
                    self.compile_node(body, &mut compiled_body, 0);
                }
                let func = self.functions.get_mut(self.current_function).unwrap();
                func.current_scope_id = previous_scope_id;

                let mut compiled_condition = Vec::new();
                self.compile_node(condition, &mut compiled_condition, 0);
                output_buf.extend(&compiled_condition);
                let cbl = i16::try_from(compiled_body.len()).unwrap();
                let jump_to_end = cbl.strict_add(2);
                output_buf.push((
                    encode!(JIFl, [
                        u32:u32::from(jump_to_end.cast_unsigned())
                    ]),
                    node.span,
                ));

                output_buf.extend(&compiled_body);

                let ccl = i16::try_from(compiled_condition.len()).unwrap();
                let jump_to_cond_eval = -cbl.strict_add(ccl).strict_add(1);
                output_buf.push((
                    encode!(Jump, [
                        u32:u32::from(jump_to_cond_eval.cast_unsigned())
                    ]),
                    node.span,
                ));
            }
            ASTNodeType::FunDef { .. } => {}
            ASTNodeType::FunCall { callee, args } => {
                for arg in args {
                    self.compile_node(arg, output_buf, 0);
                    output_buf.push((encode!(PArg), arg.span));
                }

                let func = self.functions.get_mut(self.current_function).unwrap();
                let fptr = func.get_var_safe(callee).unwrap_or_else(|| {
                    let fptr = self.get_func(&format!("@{callee}_0"));
                    let const_id = self.add_constant(ValueBuilder::Function(fptr));
                    output_buf.push((
                        encode!(Load, [
                            u8:0u8,
                            u16:const_id
                        ]),
                        node.span,
                    ));
                    0
                });
                output_buf.push((
                    encode!(Move, [
                        u8:fptr,
                    ]),
                    node.span,
                ));
                output_buf.push((
                    encode!(Call, [
                        u8:fptr,
                    ]),
                    node.span,
                ));
                if dest != 0 {
                    output_buf.push((
                        encode!(Move, [
                            u8:dest,
                        ]),
                        node.span,
                    ));
                }
            }
        }
    }
}
