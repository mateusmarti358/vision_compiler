#[derive(Debug, Clone)]
pub enum Token {
  ConstInt(i64),
  ConstFloat(f64),
  ConstStr(String),

  Identifier(String),
  
  Pushi,
  Pushf,
  PushStr,

  Add,
  Sub,
  Mul,
  Div,
  Pow,
  Mod,

  Print,
  Println,

  Store,
  Load,
  Drop,

  Label,

  Compare,

  JEQ,
  JLT,
  JLE,
  JNE,
  JGE,
  JGT,

  Func,
  Return,
  FuncEnd,
  Call,

  Debug,

  ProgramEnd
}

#[derive(Debug)]
pub enum LexerError {
  UnclosedQuotes
}

pub struct Lexer {
  pos: usize,
  src: Vec<char>,

  tokens: Vec<Token>
}
impl Lexer {
  pub fn new(src: String) -> Lexer {
    Lexer {
      pos: 0,
      src: src.chars().collect(),
      tokens: Vec::new()
    }
  }

  pub fn make_number(&mut self) -> Option<Token> {
    let mut num_str = String::new();
    let mut float = false;

    loop {
      if self.src[self.pos] == '.' {
        if float {
          break;
        }
        float = true;
        num_str.push('.');
      }

      if !self.src[self.pos].is_numeric() {
        break;
      }

      num_str.push(self.src[self.pos]);
      self.pos += 1;
    }

    if float {
      Some(Token::ConstFloat(num_str.parse().unwrap()))
    } else {
      Some(Token::ConstInt(num_str.parse().unwrap()))
    }
  }
  pub fn make_string(&mut self, quote: char) -> Result<Token, LexerError> {
    let mut string = String::new();

    loop {
      if self.pos >= self.src.len() {
        return Err(LexerError::UnclosedQuotes);
      }

      if self.src[self.pos] == quote {
        self.pos += 1;
        break;
      }

      string.push(self.src[self.pos]);
      self.pos += 1;
    }

    Ok(Token::ConstStr(string))
  }
  pub fn make_id_instr(&mut self) -> Token {
    let mut identifier = String::new();
    loop {
      if self.pos >= self.src.len() || !self.src[self.pos].is_alphanumeric() {
        break;
      }

      identifier.push(self.src[self.pos]);
      self.pos += 1;
    }

    match identifier.as_str() {
      "pushi" => Token::Pushi,
      "pushf" => Token::Pushf,
      "pushstr" => Token::PushStr,

      "add" => Token::Add,
      "sub" => Token::Sub,
      "mul" => Token::Mul,
      "div" => Token::Div,
      "pow" => Token::Pow,
      "mod" => Token::Mod,

      "print" => Token::Print,
      "println" => Token::Println,

      "store" => Token::Store,
      "load" => Token::Load,
      "drop" => Token::Drop,

      "label" => Token::Label,

      "cmp" => Token::Compare,

      "jne" => Token::JNE,
      "jlt" => Token::JLT,
      "jle" => Token::JLE,
      "jgt" => Token::JGT,
      "jge" => Token::JGE,
      "jeq" => Token::JEQ,

      "func" => Token::Func,
      "ret" => Token::Return,
      "fend" => Token::FuncEnd,
      "call" => Token::Call,

      "debug" => Token::Debug,

      "end" => Token::ProgramEnd,
      _ => Token::Identifier(identifier)
    }
  }

  pub fn generate_tokens(&mut self) -> Result<Vec<Token>, LexerError> {
    while self.pos < self.src.len() {
      let c = self.src[self.pos];

      if c == ';' {
        while self.src[self.pos] != '\n' {
          self.pos += 1;
        }
        self.pos += 1;
        continue;
      }

      if c.is_numeric() {
        let num = self.make_number().unwrap();
        self.tokens.push(num);
        continue;
      }

      if c.is_alphabetic() {
        let id = self.make_id_instr();
        self.tokens.push(id);
        continue;
      }

      if ['"', '\''].contains(&c) {
        self.pos += 1;
        let str = self.make_string(c)?;
        self.tokens.push(str);
        continue;
      }
      
      self.pos += 1;
    }

    Ok(self.tokens.clone())
  }

  /* pub fn tokens(&self) -> Option<&Vec<Token>> {
    if self.tokens.is_empty() {
      None
    } else {
      Some(&self.tokens)
    }
  } */
}
