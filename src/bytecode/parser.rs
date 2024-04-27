use super::lexer::Token;
use super::bytecode::Bytecode;

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
  Pushi(i64),
  Pushf(f64),
  PushStr(String),

  Add,
  Sub,
  Mul,
  Div,
  Pow,
  Mod,

  Print,
  Println,

  Store(String),
  Load(String),
  Drop(String),

  Label(String),

  Compare,

  JEQ(String),
  JLT(String),
  JLE(String),
  JNE(String),
  JGE(String),
  JGT(String),

  Func(String),
  Return,
  FuncEnd,
  Call(String),

  Debug,

  ProgramEnd
}
impl Instruction {
  pub fn to_bytecode(&self) -> Option<Vec<u8>> {
    match self {
      Instruction::Pushi(n) => {
        let mut bytes = vec![Bytecode::Pushi.as_byte()];
        bytes.append(&mut n.to_be_bytes().to_vec());
        Some(bytes)
      }
      Instruction::Pushf(n) => {
        let mut bytes = vec![Bytecode::Pushf.as_byte()];
        bytes.append(&mut n.to_be_bytes().to_vec());
        Some(bytes)
      }
      Instruction::PushStr(s) => {
        let mut bytes = vec![Bytecode::PushStr.as_byte()];
        bytes.append(&mut s.clone().as_bytes().to_vec());
        bytes.push(0);
        Some(bytes)
      }

      Instruction::Add => Some(vec![Bytecode::Add.as_byte()]),
      Instruction::Sub => Some(vec![Bytecode::Sub.as_byte()]),
      Instruction::Mul => Some(vec![Bytecode::Mul.as_byte()]),
      Instruction::Div => Some(vec![Bytecode::Div.as_byte()]),
      Instruction::Pow => Some(vec![Bytecode::Pow.as_byte()]),
      Instruction::Mod => Some(vec![Bytecode::Mod.as_byte()]),

      Instruction::Print => Some(vec![Bytecode::Print.as_byte()]),
      Instruction::Println => Some(vec![Bytecode::Println.as_byte()]),
      
      Instruction::Compare => Some(vec![Bytecode::Compare.as_byte()]),

      Instruction::Return => Some(vec![Bytecode::Return.as_byte()]),
      Instruction::FuncEnd => Some(vec![Bytecode::FuncEnd.as_byte()]),

      Instruction::Debug => Some(vec![Bytecode::Debug.as_byte()]),

      Instruction::ProgramEnd => Some(vec![Bytecode::ProgramEnd.as_byte()]),

      _ => None
    }
  }
}

#[derive(Debug)]
pub enum ParserError {
  UnexpectedToken(Token),
}

pub struct Parser {
  pos: usize,
  tokens: Vec<Token>,

  instructions: Vec<Instruction>,
}
impl Parser {
  pub fn new(tokens: Vec<Token>) -> Parser {
    Parser {
      pos: 0,
      tokens,

      instructions: Vec::new(),
    }
  }

  fn match_instr(&mut self) -> Result<Instruction, ParserError> {
    match self.tokens[self.pos] {
      Token::Pushi => {
        self.pos += 1;
        match self.tokens[self.pos] {
          Token::ConstInt(n) => {
            Ok(Instruction::Pushi(n))
          },
          _ => Err(ParserError::UnexpectedToken(self.tokens[self.pos].clone()))
        }
      }
      Token::Pushf => {
        self.pos += 1;
        match self.tokens[self.pos] {
          Token::ConstFloat(n) => {
            Ok(Instruction::Pushf(n))
          },
          _ => Err(ParserError::UnexpectedToken(self.tokens[self.pos].clone()))
        }
      }
      Token::PushStr => {
        self.pos += 1;
        match &self.tokens[self.pos] {
          Token::ConstStr(s) => {
            Ok(Instruction::PushStr(s.to_string()))
          },
          _ => Err(ParserError::UnexpectedToken(self.tokens[self.pos].clone()))
        }
      }

      Token::Add => Ok(Instruction::Add),
      Token::Sub => Ok(Instruction::Sub),
      Token::Mul => Ok(Instruction::Mul),
      Token::Div => Ok(Instruction::Div),
      Token::Pow => Ok(Instruction::Pow),
      Token::Mod => Ok(Instruction::Mod),

      Token::Print => Ok(Instruction::Print),
      Token::Println => Ok(Instruction::Println),

      Token::Store => {
        self.pos += 1;
        match &self.tokens[self.pos] {
          Token::Identifier(s) => {
            Ok(Instruction::Store(s.to_string()))
          },
          _ => Err(ParserError::UnexpectedToken(self.tokens[self.pos].clone()))
        }
      }
      Token::Load => {
        self.pos += 1;
        match &self.tokens[self.pos] {
          Token::Identifier(s) => {
            Ok(Instruction::Load(s.to_string()))
          },
          _ => Err(ParserError::UnexpectedToken(self.tokens[self.pos].clone()))
        }
      }
      Token::Drop => {
        self.pos += 1;
        match &self.tokens[self.pos] {
          Token::Identifier(s) => {
            Ok(Instruction::Drop(s.to_string()))
          },
          _ => Err(ParserError::UnexpectedToken(self.tokens[self.pos].clone()))
        }
      }

      Token::Label => {
        self.pos += 1;
        match &self.tokens[self.pos] {
          Token::Identifier(s) => {
            Ok(Instruction::Label(s.to_string()))
          },
          _ => Err(ParserError::UnexpectedToken(self.tokens[self.pos].clone()))
        }
      },

      Token::Compare => Ok(Instruction::Compare),

      Token::JEQ => {
        self.pos += 1;
        match &self.tokens[self.pos] {
          Token::Identifier(s) => {
            Ok(Instruction::JEQ(s.to_string()))
          },
          _ => Err(ParserError::UnexpectedToken(self.tokens[self.pos].clone()))
        }
      }
      Token::JLT => {
        self.pos += 1;
        match &self.tokens[self.pos] {
          Token::Identifier(s) => {
            Ok(Instruction::JLT(s.to_string()))
          },
          _ => Err(ParserError::UnexpectedToken(self.tokens[self.pos].clone()))
        }
      }
      Token::JLE => {
        self.pos += 1;
        match &self.tokens[self.pos] {
          Token::Identifier(s) => {
            Ok(Instruction::JLE(s.to_string()))
          },
          _ => Err(ParserError::UnexpectedToken(self.tokens[self.pos].clone()))
        }
      }
      Token::JNE => {
        self.pos += 1;
        match &self.tokens[self.pos] {
          Token::Identifier(s) => {
            Ok(Instruction::JNE(s.to_string()))
          },
          _ => Err(ParserError::UnexpectedToken(self.tokens[self.pos].clone()))
        }
      }
      Token::JGE => {
        self.pos += 1;
        match &self.tokens[self.pos] {
          Token::Identifier(s) => {
            Ok(Instruction::JGE(s.to_string()))
          },
          _ => Err(ParserError::UnexpectedToken(self.tokens[self.pos].clone()))
        }
      }
      Token::JGT => {
        self.pos += 1;
        match &self.tokens[self.pos] {
          Token::Identifier(s) => {
            Ok(Instruction::JGT(s.to_string()))
          },
          _ => Err(ParserError::UnexpectedToken(self.tokens[self.pos].clone()))
        }
      }

      Token::Func => {
        self.pos += 1;
        match &self.tokens[self.pos] {
          Token::Identifier(s) => {
            Ok(Instruction::Func(s.to_string()))
          },
          _ => Err(ParserError::UnexpectedToken(self.tokens[self.pos].clone()))
        }
      }
      Token::Return => Ok(Instruction::Return),
      Token::FuncEnd => Ok(Instruction::FuncEnd),
      Token::Call => {
        self.pos += 1;
        match &self.tokens[self.pos] {
          Token::Identifier(s) => {
            Ok(Instruction::Call(s.to_string()))
          },
          _ => Err(ParserError::UnexpectedToken(self.tokens[self.pos].clone()))
        }
      }

      Token::Debug => Ok(Instruction::Debug),
      
      Token::ProgramEnd => Ok(Instruction::ProgramEnd),
      _ => Err(ParserError::UnexpectedToken(self.tokens[self.pos].clone()))
    }
  }

  pub fn parse(&mut self) -> Result<Vec<Instruction>, ParserError> {
    while self.pos < self.tokens.len() {
      let instr = self.match_instr()?;
      self.instructions.push(instr);
      self.pos += 1;
    }

    Ok(self.instructions.clone())
  }
}