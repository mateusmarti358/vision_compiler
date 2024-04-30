use std::collections::{HashMap, VecDeque};

use crate::parser::Expression;

use crate::types::Type;

pub struct SymbolTable {
    vars: HashMap<String, VecDeque<Type>>,

    structs: HashMap<String, Vec<(String, Type)>>,
    enums: Vec<String>,
    // functions: HashMap<String, Type>
}
impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            vars: HashMap::new(),
            structs: HashMap::new(),
            enums: Vec::new(),
        }
    }

    pub fn set_var(&mut self, id: String, t: Type) {
        if let Some(stack) = self.vars.get_mut(&id) {
            stack.push_back(t);
        } else {
            let mut stack = VecDeque::new();
            stack.push_back(t);
            self.vars.insert(id, stack);
        }
    }
    pub fn unset_var(&mut self, id: &String) {
        self.vars.get_mut(id).unwrap().pop_back();
    }
    pub fn get_var(&self, id: &Expression) -> Option<&Type> {
        let mut get_stack = VecDeque::new();
        let mut expr = id.clone();
        loop {
            match expr {
                Expression::Identifier(id) => {
                    get_stack.push_back(id);
                    break;
                }

                Expression::Get(lexpr, rexpr) => {
                    if let Expression::Identifier(id) = *rexpr {
                        get_stack.push_back(id);
                    } else {
                        unreachable!();
                    }

                    expr = *lexpr;
                }

                _ => unreachable!(),
            }
        }

        let mut curr_type = None;
        while !get_stack.is_empty() {
            let curr = get_stack.pop_back().unwrap();

            if curr_type.is_none() {
                curr_type = self.vars.get(&curr).unwrap().back();
                continue;
            }

            if let Type::Custom(name, _) = curr_type.unwrap() {
                let fields = self.get_struct(&name).unwrap();
                curr_type = Some(&fields.iter().find(|(field, _)| *field == curr).unwrap().1);
            }
        }

        curr_type
    }

    pub fn set_struct(&mut self, name: String, fields: Vec<(String, Type)>) {
        self.structs.insert(name, fields);
    }
    pub fn get_struct(&self, name: &String) -> Option<&Vec<(String, Type)>> {
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
