use std::collections::HashMap;

use super::{bytecode::Bytecode, parser::Instruction};

pub struct BytecodeEmmiter {
  pos: usize,
  instructions: Vec<Instruction>,

  labels: HashMap<String, usize>,
  functions: HashMap<String, usize>,

  calls: HashMap<String, Vec<usize>>,
  jumps: HashMap<String, Vec<usize>>,

  vars: Vec<String>,

  is_exe: bool,
  bytecode: Vec<u8>
}
impl BytecodeEmmiter {
  pub fn new(instructions: Vec<Instruction>, is_library: bool) -> BytecodeEmmiter {
    BytecodeEmmiter {
      pos: 0,
      instructions,

      labels: HashMap::new(),
      functions: HashMap::new(),

      calls: HashMap::new(),
      jumps: HashMap::new(),

      vars: Vec::new(),

      is_exe: is_library,

      bytecode: Vec::new(),
    }
  }

  pub fn resolve_calls(&mut self) {
    for (name, calls) in &self.calls {
      let func_addr_bytes = self.functions.get(name).unwrap().to_be_bytes();
      for call_addr_idx in calls {
        for i in 0..8 {
          self.bytecode[call_addr_idx + i] = func_addr_bytes[i];
        }
      }
    }
  }
  pub fn resolve_jumps(&mut self) {
    for (name, jumps) in &self.jumps {
      let label_addr_bytes = self.labels.get(name).unwrap().to_be_bytes();
      for jump_addr_idx in jumps {
        for i in 0..8 {
          self.bytecode[jump_addr_idx + i] = label_addr_bytes[i];
        }
      }
    }
  }  
  
  pub fn func_bytecode(&mut self, name: String) -> Vec<u8> {
    let mut bytecode = Vec::new();
    let mut pos = *self.functions.get(&name).unwrap() + 1;

    loop {
      if pos >= self.instructions.len() {
        break;
      }
      if self.instructions[pos] == Instruction::FuncEnd {
        self.pos = pos;
        break;
      }

      match &self.instructions[pos] {
        Instruction::Label(label) => {
          self.labels.insert(label.to_string(), bytecode.len() + self.bytecode.len());
        }
        Instruction::Call(name) => {
          bytecode.push(Bytecode::Call.as_byte());
          self.calls.entry(name.to_string()).or_insert(Vec::new()).push(bytecode.len() + self.bytecode.len());
          for _ in 0..8 {
            bytecode.push(0);
          }
        }

        Instruction::JEQ(label) | Instruction::JLT(label) | Instruction::JLE(label)
        | Instruction::JNE(label) | Instruction::JGE(label) | Instruction::JGT(label) => {
          bytecode.push(match &self.instructions[pos] {
            Instruction::JEQ(_) => Bytecode::JEQ.as_byte(),
            Instruction::JLT(_) => Bytecode::JLT.as_byte(),
            Instruction::JLE(_) => Bytecode::JLE.as_byte(),
            Instruction::JNE(_) => Bytecode::JNE.as_byte(),
            Instruction::JGE(_) => Bytecode::JGE.as_byte(),
            Instruction::JGT(_) => Bytecode::JGT.as_byte(),
            _ => panic!("Invalid instruction"),
          });
          self.jumps.entry(label.to_string()).or_insert(Vec::new()).push(bytecode.len() + self.bytecode.len());
          for _ in 0..8 {
            bytecode.push(0);
          }
        }

        Instruction::Store(id) => {
          bytecode.push(Bytecode::Store.as_byte());

          if self.vars.contains(id) {
            let var_idx = self.vars.iter().position(|x| x == id).unwrap();
            bytecode.extend_from_slice(&var_idx.to_be_bytes());
          } else {
            let var_idx = self.vars.len();
            self.vars.push(id.to_string());
            bytecode.extend_from_slice(&var_idx.to_be_bytes());
          }
        }
        Instruction::Load(id) => {
          bytecode.push(Bytecode::Load.as_byte());

          let var_idx = self.vars.iter().position(|x| x == id).unwrap();
          bytecode.extend_from_slice(&var_idx.to_be_bytes());
        }
        Instruction::Drop(id) => {
          bytecode.push(Bytecode::Drop.as_byte());

          let var_idx = self.vars.iter().position(|x| x == id).unwrap();
          bytecode.extend_from_slice(&var_idx.to_be_bytes());

          self.vars.remove(var_idx);
        }

        _ => {
          let instr = self.instructions[pos].clone();
          
          if let Some(mut instr_bytecode) = instr.to_bytecode() {
            bytecode.append(&mut instr_bytecode);
          } else {
            panic!("Unsuported instruction: {:?}", instr);
          }
        }
      }

      pos += 1;
    }

    bytecode
  }

  pub fn emmit(&mut self) -> Option<Vec<u8>> {
    if self.is_exe {
      loop {
        if self.pos >= self.instructions.len() {
          break;
        }
        match self.instructions[self.pos].clone() {
          Instruction::Func(name) => {
            if name != "main" { continue; }

            self.functions.insert("main".to_string(), self.bytecode.len());
            let mut main_bytecode = self.func_bytecode("main".to_string());
            self.bytecode.append(&mut main_bytecode);
          }
          _ => ()
        }
        self.pos += 1;
      }
    }

    self.pos = 0;
    while self.pos < self.instructions.len() {
      match &self.instructions[self.pos] {
        Instruction::Func(name) => {
          if name != "main" {
            self.bytecode.push(Bytecode::Func.as_byte());
            self.functions.insert(name.to_string(), self.bytecode.len());

            let mut func_bytecode = self.func_bytecode(name.to_string());
            self.bytecode.append(&mut func_bytecode);
            continue;
          }
        }
        _ => ()
      }
      self.pos += 1;
    }

    self.resolve_calls();
    self.resolve_jumps();

    Some(self.bytecode.clone())
  }
}