use std::fmt;

#[derive(Debug, Copy, Clone)]
pub enum Literal<'a> {
    Nil,
    Number(f64),
    String(&'a str),
}

#[derive(Debug, Copy, Clone)]
pub enum TokenType {
    // Single-symbol tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Semicolon,
    Minus,
    Plus,
    Slash,
    Star,

    // Ambiguous single or double symbol tokens
    Assign,
    Bang,
    Neq,
    Eq,
    Gt,
    Geq,
    Lt,
    Leq,

    // Variable names, String and Numeric literals
    Identifier,
    String,
    Number,

    // Keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    EOF,
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
    column: usize,
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "[{}] ({:}:{:}) '{}'",
            self.ttype, self.line, self.column, self.lexeme
        )
    }
}

impl<'a> Token<'a> {
    pub fn new(ttype: TokenType, lexeme: &'a str, literal: Literal<'a>, line: usize, column: usize) -> Self {
        Self {
            ttype,
            lexeme,
            literal,
            line,
            column,
        }
    }
}
