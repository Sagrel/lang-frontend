use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Text,
    Number,
    Bool,
    Tuple(Vec<Type>),
    T(usize),
    Fn(Vec<Type>, Box<Type>),
    Error(&'static str),
}

impl Type {
    pub fn void() -> Type {
        Type::Tuple(Vec::new())
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Text => write!(f, "Text"),
            Type::Number => write!(f, "Number"),
            Type::Bool => write!(f, "Bool"),
            Type::Tuple(args) => {
                let args: String = args
                    .iter()
                    .map(|arg| arg.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "({})", args)
            }
            Type::T(n) => write!(f, "t{}!", n),
            Type::Fn(args, ret) => {
                let args: String = args
                    .iter()
                    .map(|arg| arg.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "({}) => {}", args, ret)
            }
            Type::Error(msg) => write!(f, "Error: {}", msg),
        }
    }
}
