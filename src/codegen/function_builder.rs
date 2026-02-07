use std::collections::HashMap;
use crate::common::Span;
use super::value::ValueBuilder;

#[derive(Debug, Clone)]
pub struct FunctionBuilder {
    pub name: String,
    pub scope: Vec<(HashMap<String, u8>, u8, usize)>,
    pub current_scope_id: usize,
    pub taken_scope_ids: Vec<usize>,
    pub body: Vec<(u32, Span)>,
    pub registers: [ValueBuilder; 64],
}

impl FunctionBuilder {
    pub fn new(name: &str, registers: [ValueBuilder; 64]) -> Self {
        Self {
            name: name.to_string(),
            scope: vec![(HashMap::new(), 4, 0)],
            current_scope_id: 0,
            taken_scope_ids: vec![0],
            body: Vec::new(),
            registers,
        }
    }

    pub fn get_var(&self, n: &str) -> u8 {
        let s = self.scope
            .iter()
            .rev()
            .find(
                |(s, _, sc)|
                s.contains_key(&format!("{n}_{sc}"))
            ).expect(&format!("tried to find variable {n}"));
        s.0[&format!("{n}_{}", s.2)]
    }

    pub fn get_var_safe(&self, n: &str) -> Option<u8> {
        let s = self.scope
            .iter()
            .rev()
            .find(
                |(s, _, sc)|
                s.contains_key(&format!("{n}_{sc}"))
            )?;
        s.0.get(&format!("{n}_{}", s.2)).copied()
    }
}