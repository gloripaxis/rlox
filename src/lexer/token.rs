use std::{fmt, rc::Rc};

#[derive(Clone)]
pub enum Literal {
    Nil,
    Number(f64),
    String(Rc<str>),
    Boolean(bool),
}

impl fmt::Debug for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Nil => write!(f, "nil"),
            Literal::Number(x) => write!(f, "number ({})", x),
            Literal::String(x) => write!(f, "string \"{}\"", x),
            Literal::Boolean(x) => write!(f, "boolean ({})", x),
        }
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Nil => write!(f, "nil"),
            Literal::Number(x) => {
                if x.fract().abs() < f64::EPSILON {
                    write!(f, "{:.0}", x)
                } else {
                    write!(f, "{}", x)
                }
            }
            Literal::String(x) => write!(f, "\"{}\"", x),
            Literal::Boolean(x) => write!(f, "{}", x),
        }
    }
}

impl PartialEq<Literal> for Literal {
    fn eq(&self, other: &Literal) -> bool {
        match (self, other) {
            (Literal::Nil, Literal::Nil) => true,
            (Literal::Nil, _) => false,
            (_, Literal::Nil) => false,
            (Literal::String(x), Literal::String(y)) => x == y,
            (Literal::String(_), _) => false,
            (_, Literal::String(_)) => false,
            (Literal::Number(x), Literal::Number(y)) => x == y,
            (Literal::Number(_), _) => false,
            (_, Literal::Number(_)) => false,
            (Literal::Boolean(x), Literal::Boolean(y)) => x == y,
        }
    }
}

impl Literal {
    pub fn is_truthy(&self) -> bool {
        match self {
            Literal::Nil => false,
            Literal::Boolean(x) => *x,
            _ => true,
        }
    }
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
    lexeme: Rc<str>,
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
    pub fn new(ttype: TokenType, lexeme: Rc<str>, literal: Literal, line: usize, column: usize) -> Self {
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

    pub fn get_lexeme(&self) -> &str {
        &self.lexeme
    }

    pub fn get_location(&self) -> (usize, usize) {
        (self.line, self.column)
    }
}
