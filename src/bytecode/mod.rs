use self::{lexer::Token, parser::Instruction};

mod bytecode;

mod lexer;

mod parser;

mod emmiter;

mod execution_frame;

pub mod interpreter;

fn lex(src: String) -> Option<Vec<Token>> {
  let mut lexer = lexer::Lexer::new(src);
  match lexer.generate_tokens() {
    Ok(tokens) => Some(tokens),
    Err(lexerr) => {
      eprintln!("Error lexing: {:?}", lexerr);
      return None
    }
  }
}

fn parse(tokens: Vec<Token>) -> Option<Vec<Instruction>> {
  let mut parser = parser::Parser::new(tokens.clone());
  match parser.parse() {
    Ok(instructions) => Some(instructions),
    Err(parseerr) => {
      eprintln!("Error parsing: {:?}", parseerr);
      return None
    }
  }
}

fn emmit(instructions: Vec<Instruction>, is_exe: bool) -> Option<Vec<u8>> {
  let mut emmiter = emmiter::BytecodeEmmiter::new(instructions, is_exe);
  emmiter.emmit()
}

pub fn executable_from_vit(src: String) -> Option<Vec<u8>> {
  let tokens = lex(src)?;
  let instructions = parse(tokens)?;
  emmit(instructions, true)
}
pub fn library_from_vit(src: String) -> Option<Vec<u8>> {
  let tokens = lex(src)?;
  let instructions = parse(tokens)?;
  emmit(instructions, false)
}