use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type {
    pub kind: TypeKind,
    pub is_const: bool,
    pub is_ref: bool,
}
impl Type {
    pub fn new(kind: TypeKind) -> Type {
        Type {
            kind,
            is_const: false,
            is_ref: false,
        }
    }
}
impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_const {
            write!(f, "const ")?;
        }

        if self.is_ref {
            write!(f, "ref ")?;
        }

        write!(f, "{}", self.kind)?;

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeKind {
    Custom(String),

    Array(Box<Type>),

    Bool,

    U8,
    U16,
    U32,
    U64,

    I8,
    I16,
    I32,
    I64,

    F32,
    F64,

    String,

    Void,
}
impl fmt::Display for TypeKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            TypeKind::Custom(id) => write!(f, "{}", id),
            TypeKind::Array(type_kind) => write!(f, "{}[]", type_kind),

            TypeKind::Bool => write!(f, "bool"),

            TypeKind::U8 => write!(f, "u8"),
            TypeKind::U16 => write!(f, "u16"),
            TypeKind::U32 => write!(f, "u32"),
            TypeKind::U64 => write!(f, "u64"),

            TypeKind::I8 => write!(f, "i8"),
            TypeKind::I16 => write!(f, "i16"),
            TypeKind::I32 => write!(f, "i32"),
            TypeKind::I64 => write!(f, "i64"),

            TypeKind::F32 => write!(f, "f32"),
            TypeKind::F64 => write!(f, "f64"),

            TypeKind::String => write!(f, "string"),

            TypeKind::Void => write!(f, "void"),
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
