use std::str::Chars;

use crate::types::Type;

#[derive(Debug)]
pub enum LexerError {
    UnknownToken(char, usize, usize),
    NextShouldBe(String),
    UnclosedQuotes,
    InvalidEscapeSequence,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenValue {
    Type(Type),

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

struct Lexer<'a> {
    ln: usize,
    col: usize,
    pos: usize,

    src: Chars<'a>,

    lookahead: Option<char>,
}
impl<'a> Lexer<'a> {
    fn new(src: &'a String) -> Self {
        let mut chars = src.chars();
        let lookahead = chars.next();
        Self {
            ln: 1,
            col: 0,
            pos: 0,
            src: chars,
            lookahead,
        }
    }

    fn make_type(str: &String, is_const: bool) -> Option<Type> {
        match str.as_str() {
            "bool" => Some(Type::Bool(is_const)),

            "f32" => Some(Type::F32(is_const)),
            "f64" => Some(Type::F64(is_const)),

            "i8" => Some(Type::I8(is_const)),
            "i16" => Some(Type::I16(is_const)),
            "i32" => Some(Type::I32(is_const)),
            "i64" => Some(Type::I64(is_const)),

            "u8" => Some(Type::U8(is_const)),
            "u16" => Some(Type::U16(is_const)),
            "u32" => Some(Type::U32(is_const)),
            "u64" => Some(Type::U64(is_const)),

            "str" => Some(Type::String(is_const)),
            "void" => Some(Type::Void),

            _ => None,
        }
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

            "const" => Some(TokenValue::Const),

            "and" => Some(TokenValue::And),
            "or" => Some(TokenValue::Or),
            "not" => Some(TokenValue::Not),

            "do" => Some(TokenValue::Do),
            "end" => Some(TokenValue::End),
            "defer" => Some(TokenValue::Defer),
            "continue" => Some(TokenValue::Continue),
            "break" => Some(TokenValue::Break),

            _ => None,
        }
    }

    fn next(&mut self) {
        self.lookahead = self.src.next();
        self.pos += 1;
        if self.lookahead == Some('\n') {
            self.ln += 1;
            self.col = 0;
        } else {
            self.col += 1;
        }
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
                        self.next();

                        let mut inline_c = String::new();

                        let mut in_str = (false, ' ');

                        let mut in_line_comment = false;

                        let mut in_comment = false;

                        let mut poss_comment = false;
                        let mut poss_out_comment = false;

                        let mut depth = 0;

                        while let Some(c) = self.lookahead {
                            inline_c.push(c);
                            self.next();

                            if in_line_comment {
                                if c == '\n' {
                                    in_line_comment = false;
                                }
                                continue;
                            }
                            if in_comment {
                                if c == '/' && poss_out_comment {
                                    in_comment = false;
                                    poss_out_comment = false;
                                }
                                if c == '*' && poss_out_comment {
                                    poss_out_comment = false;
                                }

                                if c == '*' && !poss_out_comment {
                                    poss_out_comment = true;
                                }
                                continue;
                            }

                            if in_str.0 {
                                if c == in_str.1 {
                                    in_str = (false, ' ');
                                }
                                continue;
                            } else {
                                if c == '"' {
                                    in_str = (true, '"');
                                } else if c == '\'' {
                                    in_str = (true, '\'');
                                }
                            }

                            if poss_comment {
                                if c == '*' {
                                    in_comment = true;
                                } else if c == '/' {
                                    in_line_comment = true;
                                }
                                poss_comment = false;
                                continue;
                            }
                            if c == '/' {
                                poss_comment = true;
                                continue;
                            }

                            if c == '{' {
                                depth += 1;
                            }
                            if c == '}' {
                                depth -= 1;

                                if depth >= 0 {
                                    continue;
                                }

                                self.next();
                                break;
                            }
                        }

                        inline_c.pop();

                        Some(TokenValue::InlineC(inline_c))
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

    fn make_id_type_kw(&mut self) -> Result<TokenValue, LexerError> {
        let mut tok = String::new();

        while let Some(c) = self.lookahead {
            if c.is_alphanumeric() || c == '_' {
                tok.push(c);
                self.next();
                continue;
            }
            break;
        }

        if let Some(t) = Lexer::make_type(&tok, false) {
            return Ok(TokenValue::Type(t));
        }

        if let Some(kw) = Lexer::make_keyword(&tok) {
            return Ok(kw);
        }

        return Ok(TokenValue::Identifier(tok));
    }

    fn make_token(&mut self) -> Result<TokenValue, LexerError> {
        if self.lookahead.is_none() {
            return Ok(TokenValue::Eof);
        }

        let lookahead = self.lookahead.unwrap();

        if !lookahead.is_ascii() {
            return Err(LexerError::UnknownToken(lookahead, self.ln, self.col));
        } else if lookahead.is_ascii_alphabetic() || lookahead == '_' {
            Ok(self.make_id_type_kw()?)
        } else if lookahead.is_ascii_digit() {
            Ok(self.make_number())
        } else if lookahead == '"' {
            Ok(self.make_string(true)?)
        } else if lookahead == '\'' {
            Ok(self.make_string(false)?)
        } else {
            if let Some(tok) = self.make_symbol() {
                Ok(tok)
            } else {
                Err(LexerError::UnknownToken(lookahead, self.ln, self.col))
            }
        }
    }

    fn tokenize(&mut self) -> Result<Vec<Token>, LexerError> {
        let mut tokens = Vec::new();

        while self.lookahead.is_some() {
            self.consume_whitespace();
            if self.lookahead == Some('#') {
                self.consume_comment();
            }

            let ln = self.ln;
            let col = self.col;
            let raw_pos = self.pos;

            match self.make_token() {
                Ok(tok) => {
                    let tok = Token {
                        value: tok,
                        ln,
                        col,
                        raw_pos,
                    };
                    tokens.push(tok);
                }
                Err(e) => return Err(e),
            }
        }

        tokens.push(Token {
            value: TokenValue::Eof,
            ln: self.ln,
            col: self.col,
            raw_pos: self.pos,
        });

        Ok(tokens)
    }
}

pub fn tokenize(src: String) -> Result<Vec<Token>, LexerError> {
    Lexer::new(&src).tokenize()
}
