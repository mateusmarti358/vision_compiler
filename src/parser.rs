use crate::lexer::{ Lexer, LexerError, Token, TokenValue };

use crate::symbol_table::SymbolTable;
use crate::types::{ Type, TypeKind };

#[derive(Debug)]
pub enum ParserError {
    LexerError(LexerError),
    UnexpectedToken(Token),
    UndeclaredVariable(String),
    FunctionsCannotReturnConst,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Boolean(bool),
    Integer(i32),
    Float(f32),
    String(String),

    Identifier(String),

    As(Box<Expression>, TypeKind),

    Index(Box<Expression>, Box<Expression>),
    Call(String, Vec<Box<Expression>>),
    Struct(String, Vec<Statement>),

    Add(Box<Expression>, Box<Expression>),
    Subtract(Box<Expression>, Box<Expression>),
    Multiply(Box<Expression>, Box<Expression>),
    Divide(Box<Expression>, Box<Expression>),
    Mod(Box<Expression>, Box<Expression>),

    Range(Box<Expression>, Box<Expression>),

    StaticGet(String, String),
    Get(Box<Expression>, Box<Expression>),
    Not(Box<Expression>),

    And(Box<Expression>, Box<Expression>),
    Or(Box<Expression>, Box<Expression>),

    LessThan(Box<Expression>, Box<Expression>),
    GreaterThan(Box<Expression>, Box<Expression>),
    LessEqual(Box<Expression>, Box<Expression>),
    GreaterEqual(Box<Expression>, Box<Expression>),
    Equal(Box<Expression>, Box<Expression>),
    Different(Box<Expression>, Box<Expression>),

    InlineC(String),
}
impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Boolean(b) => write!(f, "{}", b),
            Expression::Integer(i) => write!(f, "{}", i),
            Expression::Float(nf) => write!(f, "{}", nf),
            Expression::String(s) => write!(f, "\"{}\"", s),
            Expression::Identifier(s) => write!(f, "{}", s),

            Expression::Index(l, r) => write!(f, "{}[{}]", l, r),
            Expression::Call(s, args) => {
                write!(f, "{}(", s)?; // write fn name

                for (i, arg) in args.iter().enumerate() {
                    // write args
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }

                write!(f, ")")
            }

            Expression::Struct(name, vars) => {
                write!(f, "{} {{", name)?;
                let mut first = true;
                for s in vars {
                    if first {
                        first = false;
                    } else {
                        write!(f, ",")?;
                    }

                    write!(f, " {}", s)?;
                }
                write!(f, " }}")
            }

            Expression::StaticGet(s, r) => write!(f, "{}::{}", s, r),
            Expression::Get(l, r) => write!(f, "{}.{}", l, r),

            Expression::Equal(l, r) => write!(f, "{} == {}", l, r),
            Expression::Different(l, r) => write!(f, "{} != {}", l, r),
            Expression::LessThan(l, r) => write!(f, "{} < {}", l, r),
            Expression::GreaterThan(l, r) => write!(f, "{} > {}", l, r),
            Expression::LessEqual(l, r) => write!(f, "{} <= {}", l, r),
            Expression::GreaterEqual(l, r) => write!(f, "{} >= {}", l, r),

            Expression::Add(l, r) => write!(f, "{} + {}", l, r),
            Expression::Subtract(l, r) => write!(f, "{} - {}", l, r),
            Expression::Multiply(l, r) => write!(f, "{} * {}", l, r),
            Expression::Divide(l, r) => write!(f, "{} / {}", l, r),
            Expression::Mod(l, r) => write!(f, "{} % {}", l, r),

            Expression::As(e, t) => write!(f, "{} as {}", e, t),

            Expression::Range(l, r) => write!(f, "{}..{}", l, r),

            Expression::Not(e) => write!(f, "not {}", e),
            Expression::And(l, r) => write!(f, "{} and {}", l, r),
            Expression::Or(l, r) => write!(f, "{} or {}", l, r),

            Expression::InlineC(s) => write!(f, "!{{{}}}", s),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    VariadicArgument(String),
    Declaration(String, Type, Option<Expression>),

    Assign(Expression, Expression),

    Increment(Expression),
    Decrement(Expression),
    AddAssign(Expression, Expression),
    SubAssign(Expression, Expression),
    MulAssign(Expression, Expression),
    DivAssign(Expression, Expression),
    ModAssign(Expression, Expression),

    Call(Expression, Vec<Expression>),

    Defer(Box<Statement>),
    Return(Option<Expression>),
    Continue,
    Break,

    Function(
        String, // function name
        TypeKind, // return type
        Option<String>, // super name
        Option<Vec<Box<Statement>>>, // arguments
        Vec<Box<Statement>>, // body
    ),
    Struct(String, Vec<Box<Statement>>),
    Enum(String, Vec<String>),

    If(Expression, Vec<Box<Statement>>),
    IfElse(Expression, Vec<Box<Statement>>, Vec<Box<Statement>>),
    Match(
        Expression,
        Vec<(Expression, Vec<Box<Statement>>)>,
        Option<Vec<Box<Statement>>>,
    ),

    While(Expression, Vec<Box<Statement>>),
    ForIn(Expression, Expression, Vec<Box<Statement>>),

    Use(String, Option<String>),

    InlineC(String),
}
impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::VariadicArgument(s) => write!(f, "*{}", s),
            Statement::Declaration(s, t, None) => write!(f, "{}: {}", s, t),
            Statement::Declaration(s, t, Some(e)) => write!(f, "{}: {} = {}", s, t, e),

            Statement::Assign(s, e) => write!(f, "{} = {}", s, e),
            Statement::Increment(s) => write!(f, "{}++", s),
            Statement::Decrement(s) => write!(f, "{}--", s),
            Statement::AddAssign(s, e) => write!(f, "{} += {}", s, e),
            Statement::SubAssign(s, e) => write!(f, "{} -= {}", s, e),
            Statement::MulAssign(s, e) => write!(f, "{} *= {}", s, e),
            Statement::DivAssign(s, e) => write!(f, "{} /= {}", s, e),
            Statement::ModAssign(s, e) => write!(f, "{} %= {}", s, e),

            Statement::Call(s, args) => {
                write!(f, "{}(", s)?; // write fn name

                for (i, arg) in args.iter().enumerate() {
                    // write args
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }

                write!(f, ")")
            }

            Statement::Defer(s) => write!(f, "defer {}", s),

            Statement::Continue => write!(f, "continue\n"),
            Statement::Break => write!(f, "break\n"),
            Statement::Return(None) => write!(f, "return\n"),
            Statement::Return(Some(e)) => write!(f, "return = {}\n", e),

            Statement::If(cond, stmts) => {
                write!(f, "if {} do\n", cond)?;
                for s in stmts {
                    write!(f, "\t{}\n", s)?;
                }
                write!(f, "end")
            }
            Statement::IfElse(cond, true_body, else_body) => {
                write!(f, "if {} do\n", cond)?;
                for stmt in true_body {
                    write!(f, "\t{}\n", stmt)?;
                }
                write!(f, "else\n")?;
                for stmt in else_body {
                    write!(f, "\t{}\n", stmt)?;
                }
                write!(f, "end")
            }
            Statement::Match(cond, cases, default) => {
                write!(f, "match {}\n", cond)?;
                for (case, stmts) in cases {
                    write!(f, "\t{} do\n", case)?;

                    for s in stmts {
                        write!(f, "\t\t{}\n", s)?;
                    }

                    write!(f, "\tend\n")?;
                }

                if let Some(default) = default {
                    for stmt in default {
                        write!(f, "\t{}\n", stmt)?;
                    }
                }

                write!(f, "end")
            }

            Statement::While(cond, stmts) => {
                write!(f, "while {} do\n", cond)?;
                for s in stmts {
                    write!(f, "\t{}\n", s)?;
                }
                write!(f, "end")
            }
            Statement::ForIn(s, e, stmts) => {
                write!(f, "for {} in {} do\n", s, e)?;
                for s in stmts {
                    write!(f, "\t{}\n", s)?;
                }
                write!(f, "end")
            }

            // void doesnt make : void
            Statement::Function(s, TypeKind::Void, None, None, stmts) => {
                write!(f, "func {} do\n", s)?;
                for s in stmts {
                    write!(f, "\t{}\n", s)?;
                }
                write!(f, "end\n")
            }
            Statement::Function(s, TypeKind::Void, None, Some(args), stmts) => {
                let argstr = String::new();

                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }

                write!(f, "func {}({}) do\n", s, argstr)?;
                for s in stmts {
                    write!(f, "\t{}\n", s)?;
                }
                write!(f, "end\n")
            }

            Statement::Function(s, TypeKind::Void, Some(super_name), None, stmts) => {
                write!(f, "func {}::{} do\n", super_name, s)?;
                for s in stmts {
                    write!(f, "\t{}\n", s)?;
                }
                write!(f, "end\n")
            }
            Statement::Function(s, TypeKind::Void, Some(super_name), Some(args), stmts) => {
                let argstr = String::new();

                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }

                write!(f, "func {}::{}({}) do\n", super_name, s, argstr)?;
                for s in stmts {
                    write!(f, "\t{}\n", s)?;
                }
                write!(f, "end\n")
            }

            Statement::Function(s, t, None, None, stmts) => {
                write!(f, "func {}: {} do\n", s, t)?;
                for s in stmts {
                    write!(f, "\t{}\n", s)?;
                }
                write!(f, "end\n")
            }
            Statement::Function(s, t, None, Some(args), stmts) => {
                let argstr = String::new();

                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }

                write!(f, "func {}({}): {} do\n", s, argstr, t)?;
                for s in stmts {
                    write!(f, "\t{}\n", s)?;
                }
                write!(f, "end\n")
            }

            Statement::Function(s, t, Some(super_name), None, stmts) => {
                write!(f, "func {}::{}: {} do\n", super_name, s, t)?;
                for s in stmts {
                    write!(f, "\t{}\n", s)?;
                }
                write!(f, "end\n")
            }
            Statement::Function(s, t, Some(super_name), Some(args), stmts) => {
                let argstr = String::new();

                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }

                write!(f, "func {}::{}({}): {} do\n", super_name, s, argstr, t)?;
                for s in stmts {
                    write!(f, "\t{}\n", s)?;
                }
                write!(f, "end")
            }

            Statement::Struct(name, vars) => {
                write!(f, "struct {}\n", name)?;
                for s in vars {
                    write!(f, "\t{}\n", s)?;
                }
                write!(f, "end")
            }

            Statement::Enum(name, vars) => {
                write!(f, "enum {}\n", name)?;
                for s in vars {
                    write!(f, "\t{}\n", s)?;
                }
                write!(f, "end")
            }

            Statement::Use(modname, namespace) => match namespace {
                Some(ns) => write!(f, "use {} from {}\n", modname, ns),
                None => write!(f, "use {}", modname),
            },

            Statement::InlineC(s) => write!(f, "!{{{}}}", s),
        }
    }
}

struct Parser {
    lexer: Lexer,

    curr: Token,

    symbol_table: SymbolTable,
}
impl Parser {
    fn new(src: Lexer) -> Parser {
        Parser {
            lexer: src,

            curr: Token::default(),

            symbol_table: SymbolTable::new(),
        }
    }

    fn next(&mut self) -> Result<Token, ParserError> {
        self.curr = self.lexer.next_token().map_err(|e| ParserError::LexerError(e))?;
        Ok(self.curr.clone())
    }

    fn is_constexpr(&self, expr: &Expression) -> Result<bool, ParserError> {
        match expr {
            Expression::Or(lexpr, rexpr)
            | Expression::And(lexpr, rexpr)
            | Expression::Add(lexpr, rexpr)
            | Expression::Subtract(lexpr, rexpr)
            | Expression::Multiply(lexpr, rexpr)
            | Expression::Divide(lexpr, rexpr)
            | Expression::Mod(lexpr, rexpr)
            | Expression::Equal(lexpr, rexpr)
            | Expression::LessThan(lexpr, rexpr)
            | Expression::LessEqual(lexpr, rexpr)
            | Expression::GreaterThan(lexpr, rexpr)
            | Expression::GreaterEqual(lexpr, rexpr)
            | Expression::Different(lexpr, rexpr)
            | Expression::Range(lexpr, rexpr)
            | Expression::Index(lexpr, rexpr)
            | Expression::Get(lexpr, rexpr) => {
                Ok(self.is_constexpr(&lexpr)? && self.is_constexpr(&rexpr)?)
            }

            Expression::InlineC(_) => Ok(false),

            Expression::As(expr, _) => self.is_constexpr(expr),
            Expression::Not(expr) => self.is_constexpr(expr),

            Expression::Identifier(id) => {
                if let Some(var) = self
                                                          .symbol_table
                                                          .get_var(&Expression::Identifier(id.to_string())) {
                    return Ok(var.1.is_some())
                }
                Err(ParserError::UndeclaredVariable(id.to_string()))
            }

            Expression::Integer(_) => Ok(true),
            Expression::Float(_) => Ok(true),
            Expression::Boolean(_) => Ok(true),
            Expression::String(_) => Ok(true),
            Expression::Struct(_, _) => Ok(true),
            Expression::StaticGet(_, _) => Ok(true),

            Expression::Call(_, _) => Ok(false)
        }
    }
    fn _eval_constexpr(&self, expr: &Expression) -> Option<Expression> {
        //match expr {}
        Some(expr.clone())
    }

    fn make_type(&mut self) -> Result<Type, ParserError> {
        let mut t = Type {
            kind: TypeKind::Void,
            is_const: false,
            is_ref: false,
        };

        t.is_const = self.curr.get_value() == TokenValue::Const;
        if t.is_const {
            self.next()?;
        }

        t.is_ref = self.curr.get_value() == TokenValue::Ref;
        if t.is_ref {
            self.next()?;
        }

        match self.curr.get_value() {
            TokenValue::Type(kind) => t.kind = kind,
            TokenValue::Identifier(tid) => t.kind = TypeKind::Custom(tid),
            TokenValue::InlineC(ct) => t.kind = TypeKind::Custom(ct),
            _ => return Err(ParserError::UnexpectedToken(self.curr.clone())),
        };

        if self.next()?.get_value() == TokenValue::OpenBracket {
            if self.next()?.get_value() != TokenValue::CloseBracket {
                return Err(ParserError::UnexpectedToken(self.curr.clone()))
            }
            self.next()?;
            t.kind = TypeKind::Array(Box::new(t.clone()));
        }

        return Ok(t)
    }

    fn parse_expr(&mut self) -> Result<Expression, ParserError> {
        let lhs = self.parse_comparison()?;

        match self.curr.get_value() {
            TokenValue::And => {
                self.next()?;
                let rhs = self.parse_comparison()?;
                Ok(Expression::And(Box::new(lhs), Box::new(rhs)))
            }
            TokenValue::Or => {
                self.next()?;
                let rhs = self.parse_comparison()?;
                Ok(Expression::Or(Box::new(lhs), Box::new(rhs)))
            }
            _ => Ok(lhs),
        }
    }
    fn parse_comparison(&mut self) -> Result<Expression, ParserError> {
        let lhs = self.parse_term()?;

        match self.curr.get_value() {
            TokenValue::LessThan => {
                self.next()?;
                let rhs = self.parse_term()?;
                Ok(Expression::LessThan(Box::new(lhs), Box::new(rhs)))
            }
            TokenValue::GreaterThan => {
                self.next()?;
                let rhs = self.parse_term()?;
                Ok(Expression::GreaterThan(Box::new(lhs), Box::new(rhs)))
            }
            TokenValue::LessEqual => {
                self.next()?;
                let rhs = self.parse_term()?;
                Ok(Expression::LessEqual(Box::new(lhs), Box::new(rhs)))
            }
            TokenValue::GreaterEqual => {
                self.next()?;
                let rhs = self.parse_term()?;
                Ok(Expression::GreaterEqual(Box::new(lhs), Box::new(rhs)))
            }
            TokenValue::Equal => {
                self.next()?;
                let rhs = self.parse_term()?;
                Ok(Expression::Equal(Box::new(lhs), Box::new(rhs)))
            }
            TokenValue::NotEqual => {
                self.next()?;
                let rhs = self.parse_term()?;
                Ok(Expression::Different(Box::new(lhs), Box::new(rhs)))
            }
            _ => Ok(lhs),
        }
    }
    fn parse_term(&mut self) -> Result<Expression, ParserError> {
        let mut lhs = self.parse_factor()?;
        loop {
            match self.curr.get_value() {
                TokenValue::Plus => {
                    self.next()?;
                    let rhs = self.parse_factor()?;
                    lhs = Expression::Add(Box::new(lhs), Box::new(rhs));
                }
                TokenValue::Minus => {
                    self.next()?;
                    let rhs = self.parse_factor()?;
                    lhs = Expression::Subtract(Box::new(lhs), Box::new(rhs));
                }
                _ => break, // No more terms, break the loop
            }
        }
        Ok(lhs)
    }
    fn parse_factor(&mut self) -> Result<Expression, ParserError> {
        let mut lhs = self.parse_unary()?;
        loop {
            match self.curr.get_value() {
                TokenValue::Star => {
                    self.next()?;
                    let rhs = self.parse_unary()?;
                    lhs = Expression::Multiply(Box::new(lhs), Box::new(rhs));
                }

                TokenValue::Divide => {
                    self.next()?;
                    let rhs = self.parse_unary()?;
                    lhs = Expression::Divide(Box::new(lhs), Box::new(rhs));
                }
                TokenValue::Modulo => {
                    self.next()?;
                    let rhs = self.parse_unary()?;
                    lhs = Expression::Mod(Box::new(lhs), Box::new(rhs));
                }

                TokenValue::Range => {
                    self.next()?;
                    let rhs = self.parse_unary()?;
                    lhs = Expression::Range(Box::new(lhs), Box::new(rhs));
                }

                TokenValue::As => {
                    let right = match self.next()?.get_value() {
                        TokenValue::Type(t) => t,
                        TokenValue::Identifier(id) => TypeKind::Custom(id),

                        _ => return Err(ParserError::UnexpectedToken(self.curr.clone())),
                    };
                    self.next()?;

                    lhs = Expression::As(Box::new(lhs), right);
                }

                _ => break,
            }
        }
        Ok(lhs)
    }
    fn parse_unary(&mut self) -> Result<Expression, ParserError> {
        match self.curr.get_value() {
            TokenValue::True => {
                self.next()?;
                Ok(Expression::Boolean(true))
            }
            TokenValue::False => {
                self.next()?;
                Ok(Expression::Boolean(false))
            }

            TokenValue::Integer(i) => {
                self.next()?;
                Ok(Expression::Integer(i))
            }
            TokenValue::Float(f) => {
                self.next()?;
                Ok(Expression::Float(f))
            }
            TokenValue::String(s) => {
                self.next()?;
                Ok(Expression::String(s))
            }
            TokenValue::Identifier(id) => self.parse_identifier_as_expression(id),

            TokenValue::OpenParenthesis => {
                let expr = self.parse_expr()?;
                self.next()?; // Consume the closing parenthesis
                Ok(expr)
            }

            TokenValue::Minus => {
                let expr = self.parse_unary()?;
                Ok(Expression::Not(Box::new(expr)))
            }

            TokenValue::InlineC(inline_c) => {
                self.next()?;
                Ok(Expression::InlineC(inline_c))
            }
            _ => Err(ParserError::UnexpectedToken(self.curr.clone())),
        }
    }

    fn make_left_expr(&mut self, id: String) -> Result<Expression, ParserError> {
        match self.curr.get_value() {
            TokenValue::Dot => {
                let field = match self.next()?.get_value() {
                    TokenValue::Identifier(field) => field,
                    _ => return Err(ParserError::UnexpectedToken(self.curr.clone())),
                };

                self.next()?;

                Ok(Expression::Get(
                    Box::new(Expression::Identifier(id)),
                    Box::new(self.make_left_expr(field)?),
                ))
            }

            TokenValue::OpenBracket => {
                self.next()?;
                let index = self.parse_expr()?;
                self.next()?;
                Ok(Expression::Index(
                    Box::new(Expression::Identifier(id)),
                    Box::new(index),
                ))
            }

            TokenValue::OpenParenthesis => Ok(Expression::Identifier(id)),

            TokenValue::Assign => Ok(Expression::Identifier(id)),

            _ => Err(ParserError::UnexpectedToken(self.curr.clone())),
        }
    }

    fn make_declaration(&mut self, id: String) -> Result<Statement, ParserError> {
        if self.next()?.get_value() != TokenValue::Colon {
            return Err(ParserError::UnexpectedToken(self.curr.clone()));
        }

        self.next()?;
        let t = self.make_type()?;

        if self.curr.get_value() != TokenValue::Assign {
            return Ok(Statement::Declaration(id, t, None));
        }

        self.next()?;
        let expr = self.parse_expr()?;

        if self.is_constexpr(&expr)? {
            self.symbol_table.set_var(&id, &t, None); // TODO: self._eval_constexpr(&expr)
        } else {
            self.symbol_table.set_var(&id, &t, None);
        }

        Ok(Statement::Declaration(id, t, Some(expr)))
    }
    fn make_use(&mut self) -> Result<Statement, ParserError> {
        let mod_name = match self.next()?.get_value() {
            TokenValue::Identifier(mod_name) => mod_name,
            TokenValue::String(mod_name) => mod_name,

            TokenValue::Type(TypeKind::Bool) => "bool".to_string(),
            TokenValue::Type(TypeKind::String) => "str".to_string(),
            _ => return Err(ParserError::UnexpectedToken(self.curr.clone())),
        };

        if self.next()?.get_value() != TokenValue::From {
            return Ok(Statement::Use(mod_name, None));
        }

        let namespace = match self.next()?.get_value() {
            TokenValue::Identifier(id) => id,
            TokenValue::String(id) => id,
            _ => return Err(ParserError::UnexpectedToken(self.curr.clone())),
        };

        self.next()?;

        Ok(Statement::Use(mod_name, Some(namespace)))
    }
    fn make_defer(&mut self) -> Result<Statement, ParserError> {
        match self.next()?.get_value() {
            TokenValue::Identifier(id) => {
                self.next()?;
                Ok(Statement::Defer(Box::new(
                    self.parse_identifier_as_statement(id)?,
                )))
            }

            _ => Err(ParserError::UnexpectedToken(self.curr.clone())),
        }
    }

    fn parse_struct_field(&mut self) -> Result<Statement, ParserError> {
        let name = match self.curr.get_value() {
            TokenValue::Identifier(name) => name,
            _ => return Err(ParserError::UnexpectedToken(self.curr.clone())),
        };

        if self.next()?.get_value() != TokenValue::Colon {
            return Err(ParserError::UnexpectedToken(self.curr.clone()));
        }

        self.next()?;

        let expr = self.parse_expr()?;

        Ok(Statement::Assign(Expression::Identifier(name), expr))
    }

    fn parse_identifier_as_expression(&mut self, id: String) -> Result<Expression, ParserError> {
        match self.next()?.get_value() {
            TokenValue::Dot => {
                let field = match self.next()?.get_value() {
                    TokenValue::Identifier(field) => field,
                    _ => return Err(ParserError::UnexpectedToken(self.curr.clone())),
                };

                let field = self.parse_identifier_as_expression(field)?;

                Ok(Expression::Get(
                    Box::new(Expression::Identifier(id)),
                    Box::new(field),
                ))
            }

            TokenValue::DoubleColon => {
                let field = match self.next()?.get_value() {
                    TokenValue::Identifier(field) => field,
                    _ => return Err(ParserError::UnexpectedToken(self.curr.clone())),
                };

                self.next()?;

                Ok(Expression::StaticGet(id, field))
            }

            TokenValue::OpenBracket => {
                self.next()?;
                let index = self.parse_expr()?;
                self.next()?;
                Ok(Expression::Index(
                    Box::new(Expression::Identifier(id)),
                    Box::new(index),
                ))
            }
            TokenValue::OpenParenthesis => {
                self.next()?;
                let args = self.parse_boxed_call_args()?;
                Ok(Expression::Call(id, args))
            }

            TokenValue::OpenBrace => {
                self.next()?;

                let mut fields = Vec::new();

                loop {
                    match self.curr.get_value() {
                        TokenValue::CloseBrace => break,
                        TokenValue::Comma => {
                            self.next()?;
                            continue;
                        }
                        _ => (),
                    }

                    let field = self.parse_struct_field()?;

                    fields.push(field);
                }
                self.next()?;

                Ok(Expression::Struct(id, fields))
            }

            _ => Ok(Expression::Identifier(id)),
        }
    }
    fn parse_identifier_as_statement(&mut self, id: String) -> Result<Statement, ParserError> {
        match self.curr.get_value() {
            TokenValue::Colon => {
                self.next()?;

                let var_type = self.make_type()?;

                if self.curr.get_value() != TokenValue::Assign {
                    return Ok(Statement::Declaration(id, var_type, None));
                }
                
                self.next()?;
                let expr = self.parse_expr()?;

                Ok(Statement::Declaration(id, var_type, Some(expr)))
            }

            TokenValue::Increment => {
                self.next()?;
                Ok(Statement::Increment(Expression::Identifier(id)))
            }
            TokenValue::Decrement => {
                self.next()?;
                Ok(Statement::Decrement(Expression::Identifier(id)))
            }
            TokenValue::Assign => {
                self.next()?;

                let expr = self.parse_expr()?;

                Ok(Statement::Assign(Expression::Identifier(id), expr))
            }
            TokenValue::AddAssign => {
                self.next()?;

                let expr = self.parse_expr()?;

                Ok(Statement::AddAssign(Expression::Identifier(id), expr))
            }
            TokenValue::SubAssign => {
                self.next()?;

                let expr = self.parse_expr()?;

                Ok(Statement::SubAssign(Expression::Identifier(id), expr))
            }
            TokenValue::MulAssign => {
                self.next()?;

                let expr = self.parse_expr()?;

                Ok(Statement::MulAssign(Expression::Identifier(id), expr))
            }
            TokenValue::DivAssign => {
                self.next()?;

                let expr = self.parse_expr()?;

                Ok(Statement::DivAssign(Expression::Identifier(id), expr))
            }
            TokenValue::ModAssign => {
                self.next()?;

                let expr = self.parse_expr()?;

                Ok(Statement::ModAssign(Expression::Identifier(id), expr))
            }

            TokenValue::OpenBracket => {
                self.next()?;
                let index = self.parse_expr()?;
                self.next()?;

                if let TokenValue::Assign = self.curr.get_value() {
                    self.next()?;

                    let expr = self.parse_expr()?;

                    return Ok(Statement::Assign(
                        Expression::Index(Box::new(Expression::Identifier(id)), Box::new(index)),
                        expr,
                    ));
                }

                Err(ParserError::UnexpectedToken(self.curr.clone()))
            }
            TokenValue::Dot => {
                let lhs = self.make_left_expr(id)?;

                match self.curr.get_value() {
                    TokenValue::Assign => {
                        self.next()?;

                        let expr = self.parse_expr()?;

                        Ok(Statement::Assign(lhs, expr))
                    }

                    TokenValue::OpenParenthesis => {
                        self.next()?;

                        let args = self.parse_call_args()?;

                        Ok(Statement::Call(lhs, args))
                    }

                    _ => Err(ParserError::UnexpectedToken(self.curr.clone())),
                }
            }

            TokenValue::OpenParenthesis => {
                self.next()?;
                let args = self.parse_call_args()?;
                Ok(Statement::Call(Expression::Identifier(id), args))
            }

            _ => Err(ParserError::UnexpectedToken(self.curr.clone())),
        }
    }

    fn parse_boxed_call_args(&mut self) -> Result<Vec<Box<Expression>>, ParserError> {
        let mut args = Vec::new();

        loop {
            match self.curr.get_value() {
                TokenValue::CloseParenthesis => break,
                _ => {
                    let arg = self.parse_expr()?;
                    args.push(Box::new(arg));

                    match self.curr.get_value() {
                        TokenValue::Comma => {
                            self.next()?;
                        }
                        TokenValue::CloseParenthesis => break,
                        _ => return Err(ParserError::UnexpectedToken(self.curr.clone())),
                    }
                }
            }
        }
        self.next()?;

        Ok(args)
    }
    fn parse_call_args(&mut self) -> Result<Vec<Expression>, ParserError> {
        let mut args = Vec::new();

        loop {
            match self.curr.get_value() {
                TokenValue::CloseParenthesis => break,
                _ => {
                    let arg = self.parse_expr()?;
                    args.push(arg);

                    match self.curr.get_value() {
                        TokenValue::Comma => {
                            self.next()?;
                        }
                        TokenValue::CloseParenthesis => break,
                        _ => return Err(ParserError::UnexpectedToken(self.curr.clone())),
                    }
                }
            }
        }
        self.next()?;

        Ok(args)
    }

    fn parse_body(&mut self) -> Result<Vec<Box<Statement>>, ParserError> {
        let mut body = Vec::new();

        while self.curr.get_value() != TokenValue::End {
            let statement = match self.curr.get_value() {
                TokenValue::Identifier(id) => {
                    self.next()?;
                    self.parse_identifier_as_statement(id.to_string())?
                }

                TokenValue::If => self.parse_if()?,
                TokenValue::Else => break,
                TokenValue::Match => self.parse_match()?,

                TokenValue::While => self.parse_while()?,
                TokenValue::For => self.parse_for()?,

                TokenValue::Continue => {
                    self.next()?;
                    Statement::Continue
                }
                TokenValue::Break => {
                    self.next()?;
                    Statement::Break
                }

                TokenValue::Defer => self.make_defer()?,
                TokenValue::Return => {
                    if self.next()?.get_value() == TokenValue::Assign {
                        self.next()?;
                        let expr = self.parse_expr()?;
                        Statement::Return(Some(expr))
                    } else {
                        Statement::Return(None)
                    }
                }

                TokenValue::InlineC(inline_c) => {
                    self.next()?;
                    Statement::InlineC(inline_c)
                }

                _ => return Err(ParserError::UnexpectedToken(self.curr.clone())),
            };

            body.push(Box::new(statement));
        }

        if self.curr.get_value() == TokenValue::End {
            self.next()?;
        }
        Ok(body)
    }

    fn parse_fn_decl_args(&mut self) -> Result<Option<Vec<Box<Statement>>>, ParserError> {
        if self.curr.get_value() != TokenValue::OpenParenthesis {
            return Ok(None);
        }

        let mut args = Vec::new();
        let mut tok = self.next()?;

        while tok.get_value() != TokenValue::CloseParenthesis {
            match tok.get_value() {
                TokenValue::Star => {
                    if let TokenValue::Identifier(id) = self.next()?.get_value() {
                        args.push(Box::new(Statement::VariadicArgument(id)));
                        if self.next()?.get_value() != TokenValue::CloseParenthesis {
                            return Err(ParserError::UnexpectedToken(self.curr.clone()));
                        }
                        break;
                    }
                    return Err(ParserError::UnexpectedToken(self.curr.clone()));
                }

                TokenValue::Identifier(id) => {
                    let arg_dec = self.make_declaration(id)?;

                    args.push(Box::new(arg_dec));

                    tok = self.curr.clone();
                }

                TokenValue::InlineC(inline_c) => {
                    tok = self.next()?;
                    args.push(Box::new(Statement::InlineC(inline_c)));
                }

                TokenValue::Comma => {
                    tok = self.next()?;
                    continue;
                }

                TokenValue::CloseParenthesis => break,

                _ => return Err(ParserError::UnexpectedToken(tok)),
            }
        }

        self.next()?;
        Ok(Some(args))
    }

    fn get_function_name(&mut self) -> Result<(String, Option<String>), ParserError> {
        let super_name = match self.next()?.get_value() {
            TokenValue::Identifier(id) => Some(id),
            _ => return Err(ParserError::UnexpectedToken(self.curr.clone())),
        };

        if self.next()?.get_value() != TokenValue::DoubleColon {
            return Ok((super_name.unwrap(), None))
        }

        if let TokenValue::Identifier(id) = self.next()?.get_value() {
            self.next()?;
            return Ok((id, super_name));
        }

        Err(ParserError::UnexpectedToken(self.curr.clone()))
    }

    fn parse_func(&mut self) -> Result<Statement, ParserError> {
        let (name, super_name) = self.get_function_name()?;

        let args = self.parse_fn_decl_args()?;

        let ret_type = match self.curr.get_value() {
            TokenValue::Colon => match self.next()?.get_value() {
                TokenValue::Const => {
                    return Err(ParserError::FunctionsCannotReturnConst);
                }

                TokenValue::Type(t) => {
                    self.next()?;
                    t
                }

                TokenValue::Identifier(id) => {
                    self.next()?;
                    TypeKind::Custom(id)
                }

                _ => return Err(ParserError::UnexpectedToken(self.curr.clone())),
            },
            TokenValue::Do => TypeKind::Void,
            _ => return Err(ParserError::UnexpectedToken(self.curr.clone())),
        };

        // function body
        if self.curr.get_value() != TokenValue::Do {
            return Err(ParserError::UnexpectedToken(self.curr.clone()));
        }
        self.next()?;
        let body = self.parse_body()?;

        Ok(Statement::Function(name, ret_type, super_name, args, body))
    }

    fn parse_struct(&mut self) -> Result<Statement, ParserError> {
        let name = match self.next()?.get_value() {
            TokenValue::Identifier(name) => name,
            _ => return Err(ParserError::UnexpectedToken(self.curr.clone())),
        };
        self.next()?;

        let mut body = Vec::new();

        while self.curr.get_value() != TokenValue::End {
            let statement = match self.curr.get_value() {
                TokenValue::Identifier(id) => self.make_declaration(id)?,

                TokenValue::InlineC(inline_c) => {
                    self.next()?;
                    Statement::InlineC(inline_c)
                }

                _ => return Err(ParserError::UnexpectedToken(self.curr.clone())),
            };

            body.push(Box::new(statement));
        }

        self.next()?;

        Ok(Statement::Struct(name, body))
    }
    fn parse_enum(&mut self) -> Result<Statement, ParserError> {
        let name = match self.next()?.get_value() {
            TokenValue::Identifier(name) => name,
            _ => return Err(ParserError::UnexpectedToken(self.curr.clone())),
        };
        self.next()?;

        let mut items = Vec::new();

        while self.curr.get_value() != TokenValue::End {
            match self.curr.get_value() {
                TokenValue::Identifier(id) => {
                    self.next()?;
                    items.push(id);
                }
                _ => return Err(ParserError::UnexpectedToken(self.curr.clone())),
            };
        }

        self.next()?;
        Ok(Statement::Enum(name, items))
    }

    fn parse_if(&mut self) -> Result<Statement, ParserError> {
        self.next()?;
        let condition = self.parse_expr()?;

        if self.curr.get_value() != TokenValue::Do {
            return Err(ParserError::UnexpectedToken(self.curr.clone()));
        }
        self.next()?;
        let body = self.parse_body()?;

        if self.curr.get_value() != TokenValue::Else {
            return Ok(Statement::If(condition, body));
        }

        self.next()?;
        let else_body = self.parse_body()?;

        Ok(Statement::IfElse(condition, body, else_body))
    }
    fn parse_match(&mut self) -> Result<Statement, ParserError> {
        self.next()?;
        let condition = self.parse_expr()?;

        let mut cases = Vec::new();
        let mut default = None;

        while self.curr.get_value() != TokenValue::End {
            match self.curr.get_value() {
                TokenValue::Default => {
                    self.next()?;
                    default = Some(self.parse_body()?);
                }

                TokenValue::Identifier(sup_id) => {
                    if matches!(self.next()?.get_value(), TokenValue::DoubleColon) {
                        let prop_id = match self.next()?.get_value() {
                            TokenValue::Identifier(id) => id,
                            _ => return Err(ParserError::UnexpectedToken(self.curr.clone())),
                        };

                        if self.next()?.get_value() != TokenValue::Do {
                            return Err(ParserError::UnexpectedToken(self.curr.clone()));
                        }
                        self.next()?;
                        cases.push((Expression::StaticGet(sup_id, prop_id), self.parse_body()?));

                        continue;
                    }

                    return Err(ParserError::UnexpectedToken(self.curr.clone()));
                }

                _ => return Err(ParserError::UnexpectedToken(self.curr.clone())),
            }
        }

        self.next()?;

        Ok(Statement::Match(condition, cases, default))
    }

    fn parse_while(&mut self) -> Result<Statement, ParserError> {
        self.next()?;
        let condition = self.parse_expr()?;

        if self.curr.get_value() != TokenValue::Do {
            return Err(ParserError::UnexpectedToken(self.curr.clone()));
        }
        
        self.next()?;
        let body = self.parse_body()?;

        Ok(Statement::While(condition, body))
    }
    fn parse_for(&mut self) -> Result<Statement, ParserError> {
        let id = match self.next()?.get_value() {
            TokenValue::Identifier(id) => id,
            _ => return Err(ParserError::UnexpectedToken(self.curr.clone())),
        };

        if self.next()?.get_value() == TokenValue::In {
            self.next()?;

            let range = self.parse_expr()?;

            if self.curr.get_value() != TokenValue::Do {
                return Err(ParserError::UnexpectedToken(self.curr.clone()));
            }
            self.next()?;
            let body = self.parse_body()?;

            return Ok(Statement::ForIn(Expression::Identifier(id), range, body));
        }

        todo!()
    }

    fn parse(&mut self) -> Result<Vec<Statement>, ParserError> {
        let mut ast = Vec::new();

        self.next()?;

        while self.curr.get_value() != TokenValue::Eof {
            let statement = match self.curr.get_value() {
                TokenValue::Func => self.parse_func()?,
                TokenValue::Struct => self.parse_struct()?,
                TokenValue::Enum => self.parse_enum()?,

                TokenValue::Use => self.make_use()?,

                TokenValue::InlineC(inline_c) => {
                    self.next()?;
                    Statement::InlineC(inline_c)
                }

                _ => return Err(ParserError::UnexpectedToken(self.curr.clone())),
            };

            ast.push(statement);
        }

        Ok(ast)
    }
}

pub fn parse(lexer: Lexer) -> Result<(Lexer, Vec<Statement>), ParserError> {
    let mut parser = Parser::new(lexer);

    let ast = parser.parse()?;

    Ok((parser.lexer, ast))
}
