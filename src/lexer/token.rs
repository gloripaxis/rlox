use std::fmt;

#[derive(Debug, Copy, Clone)]
pub enum Literal<'a> {
    Nil,
    Number(f64),
    String(&'a str),
}

#[derive(Debug, Copy, Clone)]
pub enum TokenType {
    LeftParen, RightParen, LeftBrace, RightBrace,
    Comma, Dot, Semicolon,
    Minus, Plus, Slash, Star,

    Assign, 
    Bang, Neq, Eq, Gt, Geq, Lt, Leq,

    Identifier, String, Number,

    And, Class, Else, False, Fun, For, If, Nil,
    Or, Print, Return, Super, This, True, Var, While,

    EOF
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = format!("{:?}", self);
        write!(f, "{:>10}", s)
    }
}

#[derive(Debug, Clone)]
pub struct Token<'a> {
    ttype: TokenType,
    lexeme: &'a str,
    literal: Literal<'a>,
    line: usize,
}

impl <'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}::{:0>2}] '{}'", self.ttype, self.line, self.lexeme)
    }
}

impl <'a> Token<'a> {
    pub fn new(ttype: TokenType, lexeme: &'a str, literal: Literal<'a>, line: usize) -> Self {
        Self { ttype, lexeme, literal, line }
    }
}