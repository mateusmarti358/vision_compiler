use std::{collections::HashMap, i32};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Custom(String, bool),

    Array(Box<Type>, bool),

    Bool(bool),

    U8(bool),
    U16(bool),
    U32(bool),
    U64(bool),

    I8(bool),
    I16(bool),
    I32(bool),
    I64(bool),

    F32(bool),
    F64(bool),

    String(bool),

    Void,
}
impl Type {
    pub fn set_constant(&mut self, new_is_const: bool) {
        match self {
            Type::Custom(_, is_const) => *is_const = new_is_const,

            Type::Array(_, is_const) => *is_const = new_is_const,

            Type::Bool(is_const) => *is_const = new_is_const,

            Type::U8(is_const) => *is_const = new_is_const,
            Type::U16(is_const) => *is_const = new_is_const,
            Type::U32(is_const) => *is_const = new_is_const,
            Type::U64(is_const) => *is_const = new_is_const,

            Type::I8(is_const) => *is_const = new_is_const,
            Type::I16(is_const) => *is_const = new_is_const,
            Type::I32(is_const) => *is_const = new_is_const,
            Type::I64(is_const) => *is_const = new_is_const,

            Type::F32(is_const) => *is_const = new_is_const,
            Type::F64(is_const) => *is_const = new_is_const,

            Type::String(is_const) => *is_const = new_is_const,

            Type::Void => {}
        }
    }
}
impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Custom(s, false) => write!(f, "{}", s),

            Type::Array(t, false) => write!(f, "{}[]", t),

            Type::Bool(false) => write!(f, "bool"),

            Type::U8(is_const) => write!(f, "{}u8", if *is_const { "const " } else { "" }),
            Type::U16(is_const) => write!(f, "{}u16", if *is_const { "const " } else { "" }),
            Type::U32(is_const) => write!(f, "{}u32", if *is_const { "const " } else { "" }),
            Type::U64(is_const) => write!(f, "{}u64", if *is_const { "const " } else { "" }),

            Type::I8(is_const) => write!(f, "{}i8", if *is_const { "const " } else { "" }),
            Type::I16(is_const) => write!(f, "{}i16", if *is_const { "const " } else { "" }),
            Type::I32(is_const) => write!(f, "{}i32", if *is_const { "const " } else { "" }),
            Type::I64(is_const) => write!(f, "{}i64", if *is_const { "const " } else { "" }),

            Type::F32(is_const) => write!(f, "{}f32", if *is_const { "const " } else { "" }),
            Type::F64(is_const) => write!(f, "{}f64", if *is_const { "const " } else { "" }),

            Type::String(false) => write!(f, "str"),
            Type::Void => write!(f, "void"),

            Type::Custom(s, true) => write!(f, "const {}", s),

            Type::Array(t, true) => write!(f, "const {}[]", t),

            Type::Bool(true) => write!(f, "const bool"),

            Type::String(true) => write!(f, "const str"),
        }
    }
}
#[derive(Debug, Clone)]
pub enum Value {
    Custom(Box<HashMap<String, Value>>),
    Array(Box<Vec<Value>>),

    Bool(bool),

    Int(i32),
    UInt(u32),
    Float(f32),

    String(String),
}
