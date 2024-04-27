#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Value {
  String(String),

  Integer(i64),
  Float(f64),

  //Bool(bool),
}
impl std::fmt::Display for Value {
  fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Value::String(s) => write!(fmt, "{}", s),
      Value::Integer(i) => write!(fmt, "{}", i),
      Value::Float(f) => write!(fmt, "{}", f),
      //Value::Bool(b) => write!(fmt, "{}", b)
    }
  }
}

pub struct ExecutionFrame {
  addr: usize,
  vars: Vec<Value>
}
impl ExecutionFrame {
  pub fn new(addr: usize, vars: Vec<Value>) -> Self {
    Self {
      addr,
      vars
    }
  }

  pub fn load(&self) -> (usize, Vec<Value>) {
    (self.addr, self.vars.clone())
  }
}
