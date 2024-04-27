use std::collections::{ VecDeque, HashMap };

use crate::types::Type;

pub struct SymbolTable {
  symbols: HashMap<String, VecDeque<Type>>,
  structs: HashMap<String, Vec<(String, Type)>>,
  enums: Vec<String>
}
impl SymbolTable {
  pub fn new() -> SymbolTable {
    SymbolTable { symbols: HashMap::new(), structs: HashMap::new(), enums: Vec::new() }
  }

  pub fn set_symbol(&mut self, id: &String, t: Type) {
    if let Some(stack) = self.symbols.get_mut(id) {
      stack.push_back(t);
    } else {
      let mut stack = VecDeque::new();
      stack.push_back(t);
      self.symbols.insert(id.clone(), stack);
    }
  }
  pub fn unset_symbol(&mut self, id: &String) {
    self.symbols.get_mut(id).unwrap().pop_back();
  }
  pub fn get_symbol(&mut self, id: &String) -> Option<&Type> {
    if let Some(stack) = self.symbols.get(id) {
      return stack.back();
    }
    None
  }
  
  pub fn set_struct(&mut self, name: String, fields: Vec<(String, Type)>) {
    self.structs.insert(name, fields);
  }
  pub fn get_struct(&mut self, name: &String) -> Option<&Vec<(String, Type)>> {
    self.structs.get(name)
  }

  pub fn set_enum(&mut self, name: String) {
    self.enums.push(name);
  }
  pub fn is_enum(&self, name: &String) -> bool {
    if self.enums.contains(name) {
      true
    } else {
      false
    }
  }
}
