use crate::vm::value::Value;

#[derive(Debug, Clone, PartialEq)]
pub enum ValueBuilder {
    Int(i64),
    Float(f64),
    Bool(bool),
    Unit,
    String(Vec<u8>),
    Function(usize),
}

impl ValueBuilder {
    pub fn is_large(&self) -> bool {
        match self {
            Self::Int(_) | Self::Float(_) | Self::Bool(_) | Self::Unit | Self::Function(_) => false,
            Self::String(_) => true,
        }
    }

    pub fn size(&self) -> usize {
        match self {
            Self::Int(_) | Self::Float(_) | Self::Function(_) => 8,
            Self::Bool(_) | Self::Unit => 1,
            Self::String(chars) => chars.len() + 4,
        }
    }

    pub fn align(&self) -> usize {
        match self {
            Self::Int(_) | Self::Float(_) | Self::Function(_) => 8,
            Self::String(_) => 4,
            Self::Bool(_) | Self::Unit => 1,
        }
    }

    pub fn data(&self) -> Vec<u8> {
        match self {
            Self::Int(_) | Self::Float(_) | Self::Bool(_) | Self::Unit | Self::Function(_) => {
                vec![]
            }
            Self::String(chars) => {
                let mut t = Vec::with_capacity(4 + chars.len());
                t.extend(u32::try_from(chars.len()).unwrap().to_le_bytes());
                t.extend(chars);
                t
            }
        }
    }

    pub fn to_value(&self) -> Value {
        match self {
            Self::Int(n) => Value::from_int(*n),
            Self::Float(n) => Value::from_float(*n),
            Self::Bool(n) => Value::from_bool(*n),
            Self::Function(n) => Value::from_ptr(*n),
            _ => Value::nil(),
        }
    }
}
