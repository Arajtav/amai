#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int, Float, String, Bool, Unit, Unknown,
    Vector(Box<Type>), Tuple(Vec<Type>),
}

impl Type {
    pub fn display(&self) -> String {
        match self {
            Type::Int => "int".to_string(),
            Type::Float => "float".to_string(),
            Type::String => "string".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Unit => "()".to_string(),
            Type::Unknown => "{unknown}".to_string(),
            Type::Vector(ty) => format!("[{}]", ty.display()),
            Type::Tuple(ty) => format!("({})", ty.iter().map(|s| format!("{}, ", s.display())).collect::<Vec<_>>().join("")),
        }
    }
}