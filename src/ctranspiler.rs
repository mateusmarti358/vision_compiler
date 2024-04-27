use crate::lexer::tokenize;
use crate::parser::{ Expression, Statement, parse };
use crate::symbol_table::SymbolTable;
use crate::types::Type;

use crate::{ CompilationError, read_src };

fn c_type(t: &Type, symbol_table: &SymbolTable) -> String {
  match t {
    Type::Custom(s, false) => {
      if symbol_table.is_enum(s) {
        return format!("{}", s)
      }

      format!("{s}*")
    }
    Type::Array(t, false) => format!("{}[]", c_type(t, symbol_table)),

    Type::Bool(false) => "bool".to_string(),
    Type::Int(false, false) => "long long".to_string(),
    Type::Int(true, false) => "unsigned long long".to_string(),
    Type::Float(false) => "float".to_string(),
    Type::String(false) => "char*".to_string(),
    Type::Void => "void".to_string(),

    Type::Custom(s, true) => format!("const {s}*"),
    Type::Array(t, true) => format!("const {}[]", c_type(t, symbol_table)),
    Type::Bool(true) => "const bool".to_string(),
    Type::Int(false, true) => "const long long".to_string(),
    Type::Int(true, true) => "const unsigned long long".to_string(),
    Type::Float(true) => "const float".to_string(),
    Type::String(true) => "const char*".to_string(),
  }
}
fn expression_to_c(expr: &Expression, symbol_table: &SymbolTable) -> String {
  match expr {
    Expression::Boolean(b) => format!("{}", b),
    Expression::Integer(i) => format!("{}", i),
    Expression::Float(nf) => format!("{}", nf),
    Expression::String(s) => format!("\"{}\"", s),
    Expression::Identifier(s) => format!("{}", s),

    Expression::Index(l, r) => format!("{}[{}]", expression_to_c(l, symbol_table), expression_to_c(r, symbol_table)),
    Expression::StaticGet(s, r) => format!("{}::{}", s, r),
    Expression::Get(l, r) => {
      match &**r {
        Expression::Call(method, args) => {
          let mut args_str = String::new();

          for arg in args {
            args_str += ", ";
            args_str += &expression_to_c(&arg, symbol_table);
          }

          format!("{}({}{})", method, expression_to_c(l, symbol_table), args_str)
        }
        _ => format!("{}->{}", expression_to_c(l, symbol_table), expression_to_c(r, symbol_table)),
      }
    }

    Expression::Equal(l, r) => format!("{} == {}", expression_to_c(l, symbol_table), expression_to_c(r, symbol_table)),
    Expression::Different(l, r) => format!("{} != {}", expression_to_c(l, symbol_table), expression_to_c(r, symbol_table)),
    Expression::LessThan(l, r) => format!("{} < {}", expression_to_c(l, symbol_table), expression_to_c(r, symbol_table)),
    Expression::GreaterThan(l, r) => format!("{} > {}", expression_to_c(l, symbol_table), expression_to_c(r, symbol_table)),
    Expression::LessEqual(l, r) => format!("{} <= {}", expression_to_c(l, symbol_table), expression_to_c(r, symbol_table)),
    Expression::GreaterEqual(l, r) => format!("{} >= {}", expression_to_c(l, symbol_table), expression_to_c(r, symbol_table)),

    Expression::Add(l, r) => format!("{} + {}", expression_to_c(l, symbol_table), expression_to_c(r, symbol_table)),
    Expression::Subtract(l, r) => format!("{} - {}", expression_to_c(l, symbol_table), expression_to_c(r, symbol_table)),
    Expression::Multiply(l, r) => format!("{} * {}", expression_to_c(l, symbol_table), expression_to_c(r, symbol_table)),
    Expression::Divide(l, r) => format!("{} / {}", expression_to_c(l, symbol_table), expression_to_c(r, symbol_table)),
    Expression::Mod(l, r) => format!("{} % {}", expression_to_c(l, symbol_table), expression_to_c(r, symbol_table)),

    Expression::As(e, t) => format!("(({}) {})", c_type(t, symbol_table), expression_to_c(e, symbol_table)),

    Expression::Range(start, end) => {
      format!("Range({}, {})", expression_to_c(start, symbol_table), expression_to_c(end, symbol_table))
    }

    Expression::Not(e) => format!("!{}", expression_to_c(e, symbol_table)),
    Expression::And(l, r) => format!("{} && {}", expression_to_c(l, symbol_table), expression_to_c(r, symbol_table)),
    Expression::Or(l, r) => format!("{} || {}", expression_to_c(l, symbol_table), expression_to_c(r, symbol_table)),

    Expression::Call(s, args) => {
      let mut out = format!("{}(", s);

      for (i, arg) in args.iter().enumerate() {
        if i > 0 {
          out.push_str(", ");
        }
        out.push_str(&expression_to_c(arg, symbol_table));
      }

      out.push(')');
      out
    }

    Expression::Struct(s, fields) => {
      let mut out = format!("new {} {{", s);
      for field in fields {
        let mut fieldstr = statement_to_c(field, symbol_table).0;
        fieldstr.pop();
        fieldstr.pop();
        out.push_str(&format!(".{},", fieldstr));
      }
      out.push_str("}");
      out
    }

    Expression::InlineC(s) => s.clone(),
  }
}
fn statement_to_c(stmt: &Statement, symbol_table: &SymbolTable) -> (String, Option<(String, Type)>) {
  match stmt {
    Statement::Declaration(name, t, expr) => {
      match expr {
        Some(expr) => (format!("{} {} = {};\n", c_type(t, symbol_table), name, expression_to_c(expr, symbol_table)), Some((name.clone(), t.clone()))),
        None => (format!("{} {};\n", c_type(t, symbol_table), name), Some((name.clone(), t.clone())))
      }
    }

    Statement::Assign(lexpr, rexpr) => (format!("{} = {};\n", expression_to_c(lexpr, symbol_table), expression_to_c(rexpr, symbol_table)), None),
    Statement::Increment(expr) => (format!("{}++;\n", expression_to_c(expr, symbol_table)), None),
    Statement::Decrement(expr) => (format!("{}--;\n", expression_to_c(expr, symbol_table)), None),
    Statement::AddAssign(lexpr, rexpr) => (format!("{} += {};\n", expression_to_c(lexpr, symbol_table), expression_to_c(rexpr, symbol_table)), None),
    Statement::SubAssign(lexpr, rexpr) => (format!("{} -= {};\n", expression_to_c(lexpr, symbol_table), expression_to_c(rexpr, symbol_table)), None),
    Statement::MulAssign(lexpr, rexpr) => (format!("{} *= {};\n", expression_to_c(lexpr, symbol_table), expression_to_c(rexpr, symbol_table)), None),
    Statement::DivAssign(lexpr, rexpr) => (format!("{} /= {};\n", expression_to_c(lexpr, symbol_table), expression_to_c(rexpr, symbol_table)), None),
    Statement::ModAssign(lexpr, rexpr) => (format!("{} %= {};\n", expression_to_c(lexpr, symbol_table), expression_to_c(rexpr, symbol_table)), None),

    Statement::Call(expr, args) => {
      if let Expression::Get(l, r) = expr {
        let mut method_expression = &**r;
        let mut self_arg = (&**l).clone();

        loop {
          match method_expression {
            Expression::Identifier(method) => {
              let mut out = format!("{}(", method);

              out += &expression_to_c(&self_arg, symbol_table);

              for arg in args.iter() {
                out += &format!(", {}", expression_to_c(arg, symbol_table));
              }

              out += ");\n";
              return (out, None)
            }

            Expression::Get(l, r) => {
              self_arg = Expression::Get(Box::new(self_arg.clone()), Box::new((&**l).clone()));
              method_expression = &**r
            }

            _ => unreachable!("Invalid expression for method call"),
          }
        }
      }

      let mut out = format!("{}(", expression_to_c(expr, symbol_table));
      for (i, arg) in args.iter().enumerate() {
        if i > 0 { out += ", "; }
        out += &format!("{}", expression_to_c(arg, symbol_table));
      }
      out += ");\n";
      (out, None)
    },

    Statement::Continue => ("continue;\n".to_string(), None),
    Statement::Break => ("break;\n".to_string(), None),

    Statement::InlineC(inline_c) => (inline_c.clone(), None),

    _ => unreachable!("can't convert to C: {}", stmt)
  }
}

struct Transpiler {
  headers: Vec<String>,

  structs: Vec<String>,
  fn_defs: Vec<String>,

  inline_c: String,

  main_body: String,
  functions: Vec<String>,

  depth: usize,
  symbol_table: SymbolTable
}
impl Transpiler {
  fn new() -> Transpiler {
    Transpiler {
      headers: Vec::new(),

      structs: Vec::new(),
      fn_defs: Vec::new(),
      
      inline_c: String::new(),

      main_body: String::new(),
      functions: Vec::new(),

      depth: 0,
      symbol_table: SymbolTable::new()
    }
  }
  fn make_depth(&self) -> String {
    let mut out = String::new();
    for _ in 0..self.depth {
      out.push('\t');
    }
    out
  }
  fn assemble(&self) -> String {
    let mut out = String::from(self.headers.clone().join(""));
    out.push('\n');

    for structure in &self.structs {
      out.push_str(structure);
    }

    for fn_def in self.fn_defs.clone() {
      out.push_str(&format!("{}\n", fn_def));
    }

    out.push_str(&self.inline_c);
    out.push('\n');

    if !self.main_body.is_empty() {
      out.push_str(&format!("int main() {}\n\treturn 0;\n}}\n\n", self.main_body));
    }

    for (idx, func) in self.functions.iter().enumerate() {
      let mut fn_def = self.fn_defs[idx].clone();
      
      fn_def.pop();
      fn_def.push(' ');

      out.push_str(&format!("{}{}\n", fn_def, func));
    }

    out
  }

  fn body(&mut self, body: &Vec<Box<Statement>>, varg: &Option<String>, deferred: Vec<Statement>) -> String {
    let mut returned = false;
    self.depth += 1;

    let mut out = String::new();
    let mut deferred_in_scope = Vec::new();

    let mut decl_curr_scope = Vec::new();

    for stmt in body {
      match stmt.as_ref() {
        Statement::Function(_, _, _, _, _) | Statement::Struct(_, _) | Statement::Use(_, _) => {
          panic!("Unexpected statement in body: {:?}", stmt);
        }

        Statement::If(cond, body) => {
          out.push_str(&self.make_depth());
          out.push_str(&format!("if({}) {{\n{}", expression_to_c(cond, &self.symbol_table), 
                                        self.body(body, varg, [deferred.clone(), deferred_in_scope.clone()].concat())));
          out.push_str(&self.make_depth());
          out.push_str("}\n");
        }
        Statement::Match(cond, cases, default) => {
          out.push_str(&self.make_depth());

          out.push_str(&format!("switch({}) {{\n", expression_to_c(cond, &self.symbol_table)));
          self.depth += 1;
          
          for (case, body) in cases {
            out.push_str(&self.make_depth());
            out.push_str(&format!("case {}:\n", expression_to_c(case, &self.symbol_table)));
            
            out.push_str(&self.body(body, varg, [deferred.clone(), deferred_in_scope.clone()].concat()));
            
            self.depth += 1;
            out.push_str(&self.make_depth());
            out.push_str("break;\n");
            self.depth -= 1;
          }

          if let Some(body) = default {
            out.push_str(&self.make_depth());
            out.push_str("default:\n");

            out.push_str(&self.body(body, varg, [deferred.clone(), deferred_in_scope.clone()].concat()));
            
            self.depth += 1;
            out.push_str(&self.make_depth());
            out.push_str("break;\n");
            self.depth -= 1;
          }

          self.depth -= 1;
          out.push_str(&self.make_depth());
          out.push_str("}\n");
        }
        Statement::While(cond, body) => {
          out.push_str(&self.make_depth());
          out.push_str(&format!("while ( {} ) {{\n{}", expression_to_c(cond, &self.symbol_table),
                                        self.body(body, varg, [deferred.clone(), deferred_in_scope.clone()].concat())));
          out.push_str(&self.make_depth());
          out.push_str("}\n");
        }
        Statement::ForIn(var, iter, body) => {
          out.push_str(&self.make_depth());
          out.push_str(&format!("for ( auto {} : {} ) {{\n{}", expression_to_c(var, &self.symbol_table), expression_to_c(iter, &self.symbol_table),
                                        self.body(body, varg, [deferred.clone(), deferred_in_scope.clone()].concat())));
          out.push_str(&self.make_depth());
          out.push_str("}\n");
        }

        Statement::Return(expr) => {
          if let Some(varg) = varg {
            out.push_str(&self.make_depth());
            out.push_str(&format!("va_end({});", varg));
          }
          for defer in &deferred {
            out.push_str(&self.make_depth());
            out.push_str(&format!("{}", statement_to_c(defer, &self.symbol_table).0));
          }
          for defer in &deferred_in_scope {
            out.push_str(&self.make_depth());
            out.push_str(&format!("{}", statement_to_c(defer, &self.symbol_table).0));
          }

          out.push_str(&self.make_depth());
          match expr {
            Some(expr) => out.push_str(&format!("return {};\n", expression_to_c(expr, &self.symbol_table))),
            None => out.push_str("return;\n")
          };
          
          returned = true;

          break;
        }

        Statement::Defer(stmt) => {
          deferred_in_scope.push(*stmt.clone())
        }
    
        _ => {
          out.push_str(&self.make_depth());
          
          let (cstmt, decl) = statement_to_c(stmt, &self.symbol_table);
          if let Some(decl) = decl {
            self.symbol_table.set_symbol(&decl.0, decl.1);
            decl_curr_scope.push(decl.0);
          }

          out.push_str(&cstmt);
        }
      }
    }

    for var in decl_curr_scope {
      self.symbol_table.unset_symbol(&var);
    }

    if !returned {
      for defer in &deferred_in_scope {
        out.push_str(&self.make_depth());
        out.push_str(&format!("{}", statement_to_c(defer, &self.symbol_table).0));
      }
    }

    self.depth -= 1;
    out
  }

  fn make_function(&mut self, name: String, ret_type: Type, super_name: Option<String>,
                              args: Option<Vec<Box<Statement>>>, body: Vec<Box<Statement>>) {
    if name == "main" {
      self.main_body = format!("{{\n{}", self.body(&body, &None, Vec::new()));
      return;
    }
    
    let mut defaults = Vec::new();

    let mut varg = None;
    let mut va_start = String::new();

    let args = match args {
      Some(args) => {
        let mut args_str = String::new();
        let mut first = true;

        if let Some(super_name) = super_name {
          args_str.push_str(&format!("{}* self, ", super_name));
        }

        for arg in args {
          if first { first = false; } else { args_str.push_str(", "); }
          match *arg {
            Statement::VariadicArgument(va_arg) => {
              varg = Some(va_arg);
              args_str.push_str("...");
            }

            Statement::Declaration(s, t, None) => {
              va_start = s.clone();
              args_str.push_str(&format!("{} {}", c_type(&t, &self.symbol_table), s))
            }
            Statement::Declaration(s, t, Some(e)) => {
              va_start = s.clone();
              args_str.push_str(&format!("{} {}", c_type(&t, &self.symbol_table), s));
              defaults.push((s, t, e));
            }
            
            Statement::InlineC(s) => args_str.push_str(&s),

            _ => panic!("Unexpected statement: {:?}", arg)
          }
        }

        args_str
      },
      None => {
        if let Some(super_name) = super_name {
          format!("{}* self", super_name)
        } else {
          String::new()
        }
      }
    };

    self.fn_defs.push(format!("{} {}({});", c_type(&ret_type, &self.symbol_table), name, args));

    let mut body_str = String::from("{\n");

    for default in defaults {
      body_str.push_str(&format!("if({} == NULL) {{\n", default.0));
      body_str.push_str(&format!("  {} = {};\n", default.0, expression_to_c(&default.2, &self.symbol_table)));
      body_str.push_str("}\n");
    }

    if let Some(varg) = &varg {
      body_str.push_str(&format!("va_list {};", varg));
      body_str.push_str(&format!("va_start({}, {});", varg, va_start));
    }
    
    body_str += &self.body(&body, &varg, Vec::new());

    body_str.push_str(&self.make_depth());
    body_str.push_str("}\n");

    self.functions.push(body_str);
  }

  fn make_struct(&mut self, name: String, fields_stmt: Vec<Box<Statement>>) {
    self.structs.push("typedef struct {\n".to_string());
    let mut fields = Vec::with_capacity(fields_stmt.len().checked_sub(2).unwrap_or(1));

    for field in fields_stmt {
      match *field {
        Statement::Declaration(id, t, None) => {
          fields.push((id.clone(), t.clone()));
          self.structs.push(format!("  {} {};\n", c_type(&t, &self.symbol_table), id))
        }
        _ => panic!("Unexpected statement: {:?}", field)
      }
    }

    self.structs.push(format!("}} {};\n\n", name));

    self.symbol_table.set_struct(name, fields);
  }
  fn make_enum(&mut self, name: String, items: Vec<String>) {
    self.structs.push(format!("typedef enum {{\n"));
    for item in items {
      self.structs.push(format!("\t{},\n", item));
    }
    self.structs.push(format!("}} {};\n\n", name));

    self.symbol_table.set_enum(name);
  }

  fn transpile(&mut self, ast: Vec<Statement>) -> Result<String, CompilationError> {
    for stmt in ast {
      match stmt {
        Statement::Function(name, ret_type, super_name, args, body) => {
          self.make_function(name, ret_type, super_name, args, body);
        }
        Statement::Struct(name, fields) => {
          self.make_struct(name, fields);
        }
        Statement::Enum(name, items) => {
          self.make_enum(name, items);
        }

        Statement::Use(mod_name, namespace) => {
          if namespace == Some("C_STD".to_string()) {
            self.headers.push(format!("#include <{}.h>\n", mod_name));
            continue;
          }
          if namespace == Some("C".to_string()) {
            self.headers.push(format!("#include \"{}\"\n", mod_name));
            continue;
          }

          let transpiled_mod = match namespace {
            Some(namespace) => transpile_from_src(format!("{}/{}.v", namespace, mod_name), false)?,
            None => transpile_from_src(format!("{}.v", mod_name), false)?,
          };

          self.headers.push(transpiled_mod);
        }

        Statement::InlineC(code) => {
          self.inline_c.push_str(&code);
        }
        _ => panic!("Unexpected statement: {:?}", stmt)
      }
    }
    Ok(self.assemble())
  }
}

pub fn transpile_from_ast(ast: Vec<Statement>) -> Result<String, CompilationError> {
  Transpiler::new().transpile(ast)
}

pub fn transpile_from_src(src: String, print: bool) -> Result<String, CompilationError> {
  let tokens = match tokenize(read_src(&src)) {
    Ok(tokens) => tokens,
    Err(e) => return Err(CompilationError::LexerError(e))
  };
  if print {
    println!("{:#?}", tokens);
  }

  let ast = match parse(tokens) {
    Ok(ast) => ast,
    Err(e) => return Err(CompilationError::ParserError(e))
  };

  if print {
    println!("{:#?}", ast);
  }

  Transpiler::new().transpile(ast)
}
