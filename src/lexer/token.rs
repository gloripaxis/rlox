use std::fmt;

#[derive(Debug, Clone)]
pub enum Literal {
    Nil,
    Number(f64),
    String(String),
    Boolean(bool),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
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

    EndOfFile,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = format!("{:?}", self);
        write!(f, "{:>10}", s)
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    ttype: TokenType,
    lexeme: String,
    literal: Literal,
    line: usize,
    column: usize,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "[{}] ({:}:{:}) '{}'",
            self.ttype, self.line, self.column, self.lexeme
        )
    }
}

impl Token {
    pub fn new(ttype: TokenType, lexeme: String, literal: Literal, line: usize, column: usize) -> Self {
        Self {
            ttype,
            lexeme,
            literal,
            line,
            column,
        }
    }

    pub fn get_type(&self) -> TokenType {
        self.ttype
    }

    pub fn get_literal(&self) -> Literal {
        self.literal.clone()
    }

    pub fn get_location(&self) -> (usize, usize) {
        (self.line, self.column)
    }
}
