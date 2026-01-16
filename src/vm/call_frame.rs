use super::function::Function;
use super::value::Value;
use std::rc::Rc;

#[derive(Clone)]
pub struct CallFrame {
    pub caller_args: Box<[Value]>,
    pub callee_args: Vec<Value>,
    pub function: Rc<Function>,
    pub registers: [Value; 64],
    pub ip: usize,
}