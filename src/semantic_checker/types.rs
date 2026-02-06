#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    String,
    Bool,
    Unit,
    Unknown,
    Vector(Box<Type>),
    Func(Vec<Type>, Box<Type>),
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int => write!(f, "int"),
            Self::Float => write!(f, "float"),
            Self::String => write!(f, "string"),
            Self::Bool => write!(f, "bool"),
            Self::Unit => write!(f, "()"),
            Self::Unknown => write!(f, "{{unknown}}"),
            Self::Vector(ty) => write!(f, "[{ty}]"),
            Self::Func(args, return_ty) => {
                write!(f, "$(")?;
                for (i, ty) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{ty}")?;
                }
                write!(f, ") -> {return_ty}")
            }
        }
    }
}
