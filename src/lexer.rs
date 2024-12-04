use crate::types::TypeKind;

use std::fs::File;
use std::io::{BufRead, BufReader, Seek};
// use std::path::Path;

#[derive(Debug)]
pub enum LexerError {
    UnknownToken(char, usize, usize),
    NextShouldBe(String),
    UnclosedQuotes,
    InvalidEscapeSequence,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenValue {
    Type(TypeKind),

    Identifier(String),

    Integer(i32),
    Float(f32),
    String(String),

    InlineC(String),

    Assign,
    Increment,
    Decrement,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,

    Plus,
    Minus,
    Star,
    Divide,
    Modulo,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessEqual,
    GreaterEqual,

    In,
    As,

    And,
    Or,
    Not,
    True,
    False,

    Const,
    Ref,

    If,
    Else,
    While,
    For,
    Match,
    Func,
    Struct,
    Enum,

    Use,
    From,

    Do,
    End,
    Defer,
    Default,
    Continue,
    Break,
    Return,

    OpenParenthesis,
    CloseParenthesis,
    OpenBracket,
    CloseBracket,
    OpenBrace,
    CloseBrace,

    Comma,
    Colon,
    DoubleColon,
    Dot,
    Range,

    Eof,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    value: TokenValue,
    ln: usize,
    col: usize,
    raw_pos: usize,
}
impl Token {
    pub fn get_value(&self) -> TokenValue {
        self.value.clone()
    }

    pub fn is_eof(&self) -> bool {
        self.value == TokenValue::Eof
    }
}
impl Default for Token {
    fn default() -> Self {
        Token {
            value: TokenValue::Eof,
            ln: 0,
            col: 0,
            raw_pos: 0,
        }
    }
}
impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "raw position: {} | line, column: {}, {} | token: {:?}",
            self.raw_pos, self.ln, self.col, self.value
        )
    }
}

fn char_at(str: &String, pos: usize) -> char {
    str.chars().nth(pos).unwrap()
}

pub struct Lexer {
    src: BufReader<File>,

    ln: usize,
    col: usize,

    line: String,
    line_len: usize,

    lookahead: Option<char>,
}
impl Lexer {
    pub fn new(src: File) -> Self {
        let mut lexer = Self {
            src: BufReader::new(src),

            ln: 0,
            col: 0,

            line: String::new(),
            line_len: 0,

            lookahead: None,
        };

        lexer.next();

        lexer
    }

    fn make_token(&mut self, tv: TokenValue) -> Token {
        Token {
            value: tv,
            ln: self.ln,
            col: self.col,
            raw_pos: self.src.stream_position().unwrap() as usize,
        }
    }

    fn next_line(&mut self) {
        self.ln += 1;
        self.col = 0;

        self.line.clear();
        match self.src.read_line(&mut self.line) {
            Ok(len) => self.line_len = len,
            Err(e) => panic!("Failed to read line: {}", e),
        }
    }
    fn next(&mut self) -> Option<char> {
        if self.col >= self.line_len {
            self.next_line();

            if self.line_len == 0 {
                self.lookahead = None;
                return None;
            }
        }
    
        self.lookahead = Some(char_at(&self.line, self.col));
        self.col += 1;

        self.lookahead
    }

    fn consume_whitespace(&mut self) {
        while let Some(c) = self.lookahead {
            if !c.is_whitespace() {
                break;
            }
            self.next();
        }
    }
    fn consume_comment(&mut self) {
        while let Some(c) = self.lookahead {
            if c == '\n' {
                self.consume_whitespace();
                break;
            }
            self.next();
        }
    }

    fn make_inline_c(&mut self) -> Option<TokenValue> {
        self.next();

        let mut inline_c = String::new();

const IN_STR: u16 = 1 << 15;
const IN_STR_DQ: u16 = 1 << 14;
const IN_LINE_COMMENT: u16 = 1 << 13;
const POSS_COMMENT: u16 = 1 << 12;
const IN_COMMENT: u16 = 1 << 11;
const POSS_OUT_COMMENT: u16 = 1 << 10;
const DEPTH_BITS: u16 = (1 << 10) - 1;

        let mut state = 0u16;

        while let Some(c) = self.lookahead {
            inline_c.push(c);
            self.next();

            // Out of Comment
            if c == '\n' && state & IN_LINE_COMMENT != 0 {
                state = state & !IN_LINE_COMMENT;
                continue;
            }
            if state & IN_COMMENT != 0 {
                if c == '/' && state & POSS_OUT_COMMENT != 0 {
                    state = state & !IN_COMMENT; // state in_comment is now false
                    state = state & !POSS_OUT_COMMENT; // state poss_out_comment is now false
                } else if c == '*' && state & POSS_OUT_COMMENT != POSS_OUT_COMMENT {
                    state = state | POSS_OUT_COMMENT; // state poss_out_comment is now true
                }
                continue;
            }

            if state & IN_STR != 0 {
                if c == if state & IN_STR_DQ != 0 { '"' } else { '\'' } {
                    state = state & !IN_STR;
                    state = state & !IN_STR_DQ;
                }
                continue;
            } else {
                if c == '"' {
                    state = state | IN_STR;
                    state = state | IN_STR_DQ;
                } else if c == '\'' {
                    state = state | IN_STR;
                    state = state & !IN_STR_DQ;
                }
            }

            if state & POSS_COMMENT != 0 {
                if c == '*' {
                    state = state | IN_COMMENT;
                } else if c == '/' {
                    state = state | IN_LINE_COMMENT;
                }
                state = state & !POSS_COMMENT;
                continue;
            }
            if c == '/' {
                state = state | POSS_COMMENT;
                continue;
            }

            if c == '{' && state & DEPTH_BITS < DEPTH_BITS { // if depth is not full
                state += 1;
            }
            if c == '}' {
                if state & DEPTH_BITS == 0 {
                    break;
                }
                state -= 1;
            }
        }

        inline_c.pop();
        
        Some(TokenValue::InlineC(inline_c))
    }
    fn make_symbol(&mut self) -> Option<TokenValue> {
        match self.lookahead {
            Some('+') => {
                self.next();
                if self.lookahead == Some('=') {
                    self.next();
                    Some(TokenValue::AddAssign)
                } else if self.lookahead == Some('+') {
                    self.next();
                    Some(TokenValue::Increment)
                } else {
                    Some(TokenValue::Plus)
                }
            }
            Some('-') => {
                self.next();
                if self.lookahead == Some('=') {
                    self.next();
                    Some(TokenValue::SubAssign)
                } else if self.lookahead == Some('-') {
                    self.next();
                    Some(TokenValue::Decrement)
                } else {
                    Some(TokenValue::Minus)
                }
            }
            Some('*') => {
                self.next();
                if self.lookahead == Some('=') {
                    self.next();
                    Some(TokenValue::MulAssign)
                } else {
                    Some(TokenValue::Star)
                }
            }
            Some('/') => {
                self.next();
                if self.lookahead == Some('=') {
                    self.next();
                    Some(TokenValue::DivAssign)
                } else {
                    Some(TokenValue::Divide)
                }
            }
            Some('%') => {
                self.next();
                if self.lookahead == Some('=') {
                    self.next();
                    Some(TokenValue::ModAssign)
                } else {
                    Some(TokenValue::Modulo)
                }
            }

            Some('.') => {
                self.next();
                if self.lookahead == Some('.') {
                    self.next();
                    Some(TokenValue::Range)
                } else {
                    Some(TokenValue::Dot)
                }
            }
            Some(',') => {
                self.next();
                Some(TokenValue::Comma)
            }

            Some(':') => {
                self.next();
                if self.lookahead == Some(':') {
                    self.next();
                    Some(TokenValue::DoubleColon)
                } else {
                    Some(TokenValue::Colon)
                }
            }

            Some('(') => {
                self.next();
                Some(TokenValue::OpenParenthesis)
            }
            Some(')') => {
                self.next();
                Some(TokenValue::CloseParenthesis)
            }

            Some('[') => {
                self.next();
                Some(TokenValue::OpenBracket)
            }
            Some(']') => {
                self.next();
                Some(TokenValue::CloseBracket)
            }

            Some('{') => {
                self.next();
                Some(TokenValue::OpenBrace)
            }
            Some('}') => {
                self.next();
                Some(TokenValue::CloseBrace)
            }

            Some('=') => {
                self.next();
                if self.lookahead == Some('=') {
                    self.next();
                    Some(TokenValue::Equal)
                } else {
                    Some(TokenValue::Assign)
                }
            }

            Some('!') => {
                self.next();
                match self.lookahead {
                    Some('=') => {
                        self.next();
                        Some(TokenValue::NotEqual)
                    }
                    Some('{') => {
                        self.make_inline_c()
                    }
                    _ => Some(TokenValue::Not),
                }
            }

            Some('<') => {
                self.next();
                if self.lookahead == Some('=') {
                    self.next();
                    Some(TokenValue::LessEqual)
                } else {
                    Some(TokenValue::LessThan)
                }
            }
            Some('>') => {
                self.next();
                if self.lookahead == Some('=') {
                    self.next();
                    Some(TokenValue::GreaterEqual)
                } else {
                    Some(TokenValue::GreaterThan)
                }
            }

            _ => None,
        }
    }

    fn make_number(&mut self) -> TokenValue {
        let mut number = String::new();
        let mut float = false;

        while let Some(c) = self.lookahead {
            if c.is_ascii_digit() {
                number.push(c);
                self.next();
            } else if c == '.' && !float {
                float = true;
                number.push(c);
                self.next();
            } else {
                break;
            }
        }

        if float {
            TokenValue::Float(number.parse().unwrap())
        } else {
            TokenValue::Integer(number.parse().unwrap())
        }
    }
    fn make_string(&mut self, double_quotes: bool) -> Result<TokenValue, LexerError> {
        self.next();
        let mut string = String::new();

        while let Some(c) = self.lookahead {
            if c == if double_quotes { '"' } else { '\'' } {
                self.next();
                return Ok(TokenValue::String(string));
            }

            string.push(c);

            self.next();
        }

        Err(LexerError::UnclosedQuotes)
    }

    fn make_type(str: &String) -> Option<TypeKind> {
        Some(match str.as_str() {
            "bool" => TypeKind::Bool,

            "f32" => TypeKind::F32,
            "f64" => TypeKind::F64,

            "i8" => TypeKind::I8,
            "i16" => TypeKind::I16,
            "i32" => TypeKind::I32,
            "i64" => TypeKind::I64,

            "u8" => TypeKind::U8,
            "u16" => TypeKind::U16,
            "u32" => TypeKind::U32,
            "u64" => TypeKind::U64,

            "str" => TypeKind::String,
            "void" => TypeKind::Void,

            _ => return None,
        })
    }
    fn make_keyword(str: &String) -> Option<TokenValue> {
        match str.as_str() {
            "true" => Some(TokenValue::True),
            "false" => Some(TokenValue::False),

            "in" => Some(TokenValue::In),
            "as" => Some(TokenValue::As),

            "if" => Some(TokenValue::If),
            "else" => Some(TokenValue::Else),
            "while" => Some(TokenValue::While),
            "for" => Some(TokenValue::For),
            "match" => Some(TokenValue::Match),

            "func" => Some(TokenValue::Func),
            "struct" => Some(TokenValue::Struct),
            "enum" => Some(TokenValue::Enum),

            "use" => Some(TokenValue::Use),
            "from" => Some(TokenValue::From),

            "default" => Some(TokenValue::Default),
            "return" => Some(TokenValue::Return),

            "and" => Some(TokenValue::And),
            "or" => Some(TokenValue::Or),
            "not" => Some(TokenValue::Not),

            "const" => Some(TokenValue::Const),
            "ref" => Some(TokenValue::Ref),

            "do" => Some(TokenValue::Do),
            "end" => Some(TokenValue::End),
            "defer" => Some(TokenValue::Defer),
            "continue" => Some(TokenValue::Continue),
            "break" => Some(TokenValue::Break),

            _ => None,
        }
    }

    fn make_id_type_kw(&mut self) -> Result<TokenValue, LexerError> {
        let mut tok = String::new();

        while let Some(c) = self.lookahead {
            if !c.is_alphanumeric() && c != '_' {
                break;
            }
            tok.push(c);
            self.next();
        }

        if let Some(t) = Lexer::make_type(&tok) {
            return Ok(TokenValue::Type(t));
        }

        if let Some(kw) = Lexer::make_keyword(&tok) {
            return Ok(kw);
        }

        return Ok(TokenValue::Identifier(tok));
    }

    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        self.consume_whitespace();
        if self.lookahead == Some('#') {
            self.consume_comment();
        }

        if self.lookahead.is_none() {
            return Ok(Token { value: TokenValue::Eof, ln: self.ln, col: self.col, raw_pos: self.col });
        }

        let lookahead = self.lookahead.unwrap();

        if !lookahead.is_ascii() {
            return Err(LexerError::UnknownToken(lookahead, self.ln, self.col));
        }
        
        if lookahead.is_ascii_alphabetic() || lookahead == '_' {
            let tv = self.make_id_type_kw()?;
            return Ok(self.make_token(tv))
        }
        
        if lookahead.is_ascii_digit() {
            let tv = self.make_number();
            return Ok(self.make_token(tv))
        }
        
        if lookahead == '"' || lookahead == '\'' {
            let tv = self.make_string(lookahead == '"')?;
            return Ok(self.make_token(tv))
        }
        
        if let Some(tok) = self.make_symbol() {
            Ok(self.make_token(tok))
        } else {
            Err(LexerError::UnknownToken(lookahead, self.ln, self.col))
        }
    }
}
