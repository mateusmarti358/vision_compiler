#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
  Custom(String, bool),
  
  Array(Box<Type>, bool),

  Bool(bool),
  
  Int(bool, bool),
  Float(bool),

  String(bool),

  Void,
}
impl Type {
  pub fn set_constant(&mut self, new_is_const: bool) {
    match self {
      Type::Custom(_, is_const) => {
        *is_const = new_is_const
      }
      Type::Array(_, is_const) => {
        *is_const = new_is_const
      }
      Type::Bool(is_const) => {
        *is_const = new_is_const
      },
      Type::Int(_, is_const) => {
        *is_const = new_is_const
      },
      Type::Float(is_const) => {
        *is_const = new_is_const
      },
      Type::String(is_const) => {
        *is_const = new_is_const
      },
      Type::Void => {  },
    }
  }
}
impl std::fmt::Display for Type {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Type::Custom(s, false) => write!(f, "{}", s),

      Type::Array(t, false) => write!(f, "{}[]", t),

      Type::Bool(false) => write!(f, "bool"),

      Type::Int(false, false) => write!(f, "int"),
      Type::Int(true, false) => write!(f, "uint"),

      Type::Float(false) => write!(f, "float"),
      Type::String(false) => write!(f, "str"),
      Type::Void => write!(f, "void"),

      Type::Custom(s, true) => write!(f, "const {}", s),
      
      Type::Array(t, true) => write!(f, "const {}[]", t),

      Type::Bool(true) => write!(f, "const bool"),

      Type::Int(false, true) => write!(f, "const int"),
      Type::Int(true, true) => write!(f, "const uint"),
      
      Type::Float(true) => write!(f, "const float"),
      Type::String(true) => write!(f, "const str"),
    }
  }
}
