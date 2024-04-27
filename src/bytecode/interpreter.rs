use std::time::Duration;

use super::bytecode::Bytecode;
use super::execution_frame::{ExecutionFrame, Value};

use crate::util::stack::Stack;

const STACK_SIZE: usize = 256;

struct BytecodeVM {
  bytecode: Vec<u8>,
  pc: usize,

  stack: Stack<Value>,
  exec_stack: Stack<ExecutionFrame>,

  vars: Vec<Value>,
}
impl BytecodeVM {
  fn new(bytecode: Vec<u8>) -> Self {
    Self {
      bytecode , pc: 0,
      stack: Stack::new(), exec_stack: Stack::new(),
      vars: Vec::new(),
    }
  }

  fn next_int(&mut self) -> i64 {
    self.pc += 7;
    i64::from_be_bytes(self.bytecode[self.pc - 7..self.pc + 1].try_into().unwrap())
  }
  fn next_float(&mut self) -> f64 {
    self.pc += 7;
    f64::from_be_bytes(self.bytecode[self.pc - 7..self.pc + 1].try_into().unwrap())
  }
  fn next_str(&mut self) -> String {
    let mut str = String::new();
    self.pc += 1;

    loop {
      if self.bytecode[self.pc] == 0x00 {
        break;
      }
      str.push(self.bytecode[self.pc] as char);
      self.pc += 1;
    }

    str
  }

  fn push_execution_frame(&mut self, addr: usize) {
    self.exec_stack.push(ExecutionFrame::new(addr, self.vars.clone()));
    self.vars.clear();
  }
  fn load_execution_frame(&mut self) {
    (self.pc, self.vars) = self.exec_stack.pop().unwrap().load();
  }

  fn binary_op(&mut self) -> Option<Value> {
    let b = self.stack.pop();
    let a = self.stack.pop();

    if let (Some(a), Some(b)) = (a, b) {
      match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => {
          match Bytecode::from_byte(self.bytecode[self.pc]) {
            Bytecode::Add => Some(Value::Integer(a + b)),
            Bytecode::Sub | Bytecode::Compare => Some(Value::Integer(a - b)),
            Bytecode::Mul => Some(Value::Integer(a * b)),
            Bytecode::Div => Some(Value::Integer(a / b)),
            Bytecode::Pow => Some(Value::Integer(a.pow(b as u32))),
            Bytecode::Mod => Some(Value::Integer(a % b)),
            _ => unreachable!()
          }
        }
        (Value::Float(a), Value::Float(b)) => {
          match Bytecode::from_byte(self.bytecode[self.pc]) {
            Bytecode::Add => Some(Value::Float(a + b)),
            Bytecode::Sub | Bytecode::Compare => Some(Value::Float(a - b)),
            Bytecode::Mul => Some(Value::Float(a * b)),
            Bytecode::Div => Some(Value::Float(a / b)),
            Bytecode::Pow => Some(Value::Float(a.powf(b))),
            Bytecode::Mod => Some(Value::Float(a % b)),
            _ => unreachable!()
          }
        }
        (Value::Integer(a), Value::Float(b)) => {
          match Bytecode::from_byte(self.bytecode[self.pc]) {
            Bytecode::Add => Some(Value::Float(a as f64 + b)),
            Bytecode::Sub | Bytecode::Compare => Some(Value::Float(a as f64 - b)),
            Bytecode::Mul => Some(Value::Float(a as f64 * b)),
            Bytecode::Div => Some(Value::Float(a as f64 / b)),
            Bytecode::Pow => Some(Value::Float((a as f64).powf(b))),
            Bytecode::Mod => Some(Value::Float(a as f64 % b)),
            _ => unreachable!()
          }
        }
        (Value::Float(a), Value::Integer(b)) => {
          match Bytecode::from_byte(self.bytecode[self.pc]) {
            Bytecode::Add => Some(Value::Float(a + b as f64)),
            Bytecode::Sub | Bytecode::Compare => Some(Value::Float(a - b as f64)),
            Bytecode::Mul => Some(Value::Float(a * b as f64)),
            Bytecode::Div => Some(Value::Float(a / b as f64)),
            Bytecode::Pow => Some(Value::Float((a as f64).powf(b as f64))),
            Bytecode::Mod => Some(Value::Float(a % b as f64)),
            _ => unreachable!()
          }
        }
        (Value::String(a), Value::String(b)) => {
          match Bytecode::from_byte(self.bytecode[self.pc]) {
            Bytecode::Add => Some(Value::String(a + &b)),
            _ => {
              eprintln!("Invalid operands for binary operation");
              None
            }
          }
        }
        _ => {
          eprintln!("Invalid operands for binary operation");
          None
        }
      }
    } else {
      eprintln!("Not enough operands");
      None
    }
  }

  fn jmp_cond(&mut self, cond: &dyn Fn(Option<Value>, Option<Value>) -> bool) -> bool {
    if cond(self.stack.pop(), Some(Value::Integer(0))) {
      self.pc = self.next_int() as usize;
      if self.pc >= self.bytecode.len() {
        eprintln!("Jump out of bounds: {}", self.pc);
        return false;
      }
      return true;
    }
    return false;
  }

  fn interpret(&mut self) {
    while self.pc < self.bytecode.len() {
      if self.stack.len() > STACK_SIZE {
        eprintln!("Stack overflow");
        return;
      }
      match Bytecode::from_byte(self.bytecode[self.pc]) {
        Bytecode::Pushi => {
          self.pc += 1;
          let val = self.next_int();
          self.stack.push(Value::Integer(val));
        }
        Bytecode::Pushf => {
          self.pc += 1;
          let val = self.next_float();
          self.stack.push(Value::Float(val));
        }
        Bytecode::PushStr => {
          let str = self.next_str();
          self.stack.push(Value::String(str));
        }

        Bytecode::Add | Bytecode::Sub | Bytecode::Mul | Bytecode::Div | Bytecode::Pow | Bytecode::Mod => {
          if let Some(val) = self.binary_op() {
            self.stack.push(val);
          } else {
            return;
          }
        }

        Bytecode::Print => {
          if let Some(val) = self.stack.pop() {
            print!("{val}");
          } else {
            eprintln!("Stack is empty <print>");
            return;
          }
        }
        Bytecode::Println => {
          if let Some(mut val) = self.stack.pop() {
            if let Value::String(val) = &mut val {
              *val = val.replace("\\n", "\n");
            }
            println!("{val}");
          } else {
            eprintln!("Stack is empty <println>");
            return;
          }
        }

        Bytecode::Store => {
          self.pc += 1;
          let idx = self.next_int() as usize;
          if idx > self.vars.len() {
            eprintln!("Index out of bounds: {} at {:#04x}", idx, self.pc);
            return;
          }

          if let Some(val) = self.stack.pop() {
            if idx == self.vars.len() {
              self.vars.push(val);
            } else {
              self.vars[idx] = val;
            }
          } else {
            eprintln!("Stack is empty");
            return;
          }
        }
        Bytecode::Load => {self.pc += 1;
          let var_idx = self.next_int() as usize;
          if let Some(val) = self.vars.get(var_idx) {
            self.stack.push(val.clone());
          } else {
            eprintln!("Index out of bounds: {}", var_idx);
            return;
          }
        }
        Bytecode::Drop => {
          self.pc += 1;

          let var_idx = self.next_int() as usize;

          if var_idx >= self.vars.len() {
            eprintln!("Index out of bounds: {}", var_idx);
            return;
          }

          self.vars.remove(var_idx);
        }

        Bytecode::Compare => {
          let res = self.binary_op();

          if let Some(val) = res {
            self.stack.push(val);
          } else {
            return;
          }
        }
        
        Bytecode::JEQ => {self.pc += 1;
          if self.jmp_cond(&|a, b| a == b) {
            continue;
          }
          self.pc += 7;
        }
        Bytecode::JNE => {self.pc += 1;
          if self.jmp_cond(&|a, b| a != b) {
            continue;
          }
          self.pc += 7;
        }
        Bytecode::JLT => {self.pc += 1;
          if self.jmp_cond(&|a, b| a < b) {
            continue;
          }
          self.pc += 7;
        }
        Bytecode::JLE => {self.pc += 1;
          if self.jmp_cond(&|a, b| a <= b) {
            continue;
          }
          self.pc += 7;
        }
        Bytecode::JGE => {self.pc += 1;
          if self.jmp_cond(&|a, b| a >= b) {
            continue;
          }
          self.pc += 7;
        }
        Bytecode::JGT => {self.pc += 1;
          if self.jmp_cond(&|a, b| a >= b) {
            continue;
          }
          self.pc += 7;
        }

        Bytecode::Return => {
          self.load_execution_frame();
        }

        Bytecode::Call => {
          self.pc += 1;
          self.push_execution_frame(self.pc + 8);

          let fn_addr = self.next_int() as usize;
          if fn_addr >= self.bytecode.len() {
            eprintln!("Call out of bounds: {}", fn_addr);
            return;
          }

          self.pc = fn_addr;
        }

        Bytecode::Debug => {
          eprintln!("debug: pc: {:#04x}", self.pc);
          eprintln!("debug: stack: {:?}", self.stack);
          eprintln!("debug: vars: {:?}", self.vars);
          std::thread::sleep(Duration::from_millis(500));
        }

        Bytecode::ProgramEnd => break,

        _ => {
          eprintln!("Unknown opcode: {:#04x} at: {:#04x}", self.bytecode[self.pc], self.pc);
        }
      }
      self.pc += 1;
    }
  }
}

pub fn interpret(bytecode: Vec<u8>) {
  BytecodeVM::new(bytecode).interpret();
}