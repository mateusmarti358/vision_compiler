use std::collections::{HashMap, VecDeque};

use crate::parser::Expression;

use crate::types::{ Type, TypeKind };

pub struct SymbolTable {
    vars: HashMap<String, VecDeque<Type>>,

    structs: HashMap<String, Vec<(String, Type)>>,
    enums: Vec<String>,
    // funcs: HashMap<(Option<String>, String), Type>,
}
impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            vars: HashMap::new(),
            structs: HashMap::new(),
            enums: Vec::new(),
            // funcs: HashMap::new(),
        }
    }

    pub fn set_var(&mut self, id: &String, t: &Type) {
        if let Some(stack) = self.vars.get_mut(id) {
            stack.push_back(t.clone());
        } else {
            let mut stack = VecDeque::new();
            stack.push_back(t.clone());
            self.vars.insert(id.to_string(), stack);
        }
    }
    pub fn unset_var(&mut self, id: &String) {
        self.vars.get_mut(id).unwrap().pop_back();
    }
    pub fn get_var(&self, id: &Expression) -> Option<Type> {
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

        let mut curr_var = None;
        while let Some(curr) = get_stack.pop_back() {
            if curr_var.is_none() {
                curr_var = self.vars.get(&curr).and_then(|list| list.back().cloned());
                continue;
            }

            let TypeKind::Custom(name) = &curr_var.clone().unwrap().kind else { continue };
            let Some(fields) = self.get_struct(&name) else { continue };
            let Some(curr_type) = fields.iter().find_map(|(field, ty)| {
                if *field == curr { Some(ty.clone()) } else { None }
            }) else { continue };
            
            curr_var = Some(curr_type);
        }

        curr_var
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

    // returns true if the function is overwrited
    // pub fn set_func(
    //     &mut self,
    //     super_name: &Option<String>,
    //     name: &String,
    //     ret_type: &Type,
    // ) -> bool {
    //     if self
    //         .funcs
    //         .get(&(super_name.clone(), name.to_string()))
    //         .is_some()
    //     {
    //         return true;
    //     }
    //     self.funcs
    //         .insert((super_name.clone(), name.to_string()), ret_type.clone());
    //     false
    // }
    // pub fn get_func(&mut self, super_name: &Option<String>, name: &String) -> Option<&Type> {
    //     self.funcs.get(&(super_name.clone(), name.to_string()))
    // }
}
