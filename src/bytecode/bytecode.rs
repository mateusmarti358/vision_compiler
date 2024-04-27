#[derive(Debug, Clone, PartialEq)]
pub enum Bytecode {
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

  Compare,
  Jmp, JEQ, JLT, JLE, JNE, JGE, JGT,

  Func,
  Return,
  FuncEnd,
  Call,

  Debug,

  ProgramEnd,
  Unknown
}
impl Bytecode {
  pub fn from_byte(byte: u8) -> Bytecode {
    match byte {
      0x01 => Bytecode::Pushi,
      0x02 => Bytecode::Pushf,
      0x03 => Bytecode::PushStr,

      0x05 => Bytecode::Add,
      0x06 => Bytecode::Sub,
      0x07 => Bytecode::Mul,
      0x08 => Bytecode::Div,
      0x09 => Bytecode::Pow,
      0x0A => Bytecode::Mod,

      0x0B => Bytecode::Print,
      0x1B => Bytecode::Println,

      0x0C => Bytecode::Store,
      0x0D => Bytecode::Load,
      0x0E => Bytecode::Drop,

      0x0F => Bytecode::Compare,
      0x10 => Bytecode::Jmp,
      0x11 => Bytecode::JEQ,
      0x12 => Bytecode::JLT,
      0x13 => Bytecode::JLE,
      0x14 => Bytecode::JNE,
      0x15 => Bytecode::JGE,
      0x16 => Bytecode::JGT,

      0x17 => Bytecode::Func,
      0x18 => Bytecode::Return,
      0x19 => Bytecode::FuncEnd,
      0x1A => Bytecode::Call,
      // 0x1B Println

      0xDB => Bytecode::Debug,

      0xFF => Bytecode::ProgramEnd,
      _ => Bytecode::Unknown
    }
  }
  pub fn as_byte(self) -> u8 {
    match self {
      Bytecode::Pushi => 0x01,
      Bytecode::Pushf => 0x02,
      Bytecode::PushStr => 0x03,

      Bytecode::Add => 0x05,
      Bytecode::Sub => 0x06,
      Bytecode::Mul => 0x07,
      Bytecode::Div => 0x08,
      Bytecode::Pow => 0x09,
      Bytecode::Mod => 0x0A,

      Bytecode::Print => 0x0B,
      Bytecode::Println => 0x1B,

      Bytecode::Store => 0x0C,
      Bytecode::Load => 0x0D,
      Bytecode::Drop => 0x0E,

      Bytecode::Compare => 0x0F,
      Bytecode::Jmp => 0x10,
      Bytecode::JEQ => 0x11,
      Bytecode::JLT => 0x12,
      Bytecode::JLE => 0x13,
      Bytecode::JNE => 0x14,
      Bytecode::JGE => 0x15,
      Bytecode::JGT => 0x16,

      Bytecode::Func => 0x17,
      Bytecode::Return => 0x18,
      Bytecode::FuncEnd => 0x19,
      Bytecode::Call => 0x1A, // 0x1B Println

      Bytecode::Debug => 0xDB,

      Bytecode::ProgramEnd => 0xFF,
      Bytecode::Unknown => 0x00
    }
  }
}
