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
            (Literal::String(x), Literal::String(y)) => x == y,
            (Literal::Number(x), Literal::Number(y)) => x == y,
            (Literal::Boolean(x), Literal::Boolean(y)) => x == y,
            _ => false,
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

impl TokenType {
    pub fn from_char(c: char) -> TokenType {
        match c {
            '(' => TokenType::LeftParen,
            ')' => TokenType::RightParen,
            '{' => TokenType::LeftBrace,
            '}' => TokenType::RightBrace,
            ',' => TokenType::Comma,
            '.' => TokenType::Dot,
            '-' => TokenType::Minus,
            '+' => TokenType::Plus,
            ';' => TokenType::Semicolon,
            '*' => TokenType::Star,
            '!' => TokenType::Bang,
            '=' => TokenType::Assign,
            '<' => TokenType::Lt,
            '>' => TokenType::Gt,
            _ => unreachable!("Method only called from Lexer for single-character tokens!"),
        }
    }

    pub fn from_pre_eq_char(c: char) -> TokenType {
        match c {
            '!' => TokenType::Neq,
            '=' => TokenType::Eq,
            '<' => TokenType::Leq,
            '>' => TokenType::Geq,
            _ => unreachable!("Method only called from Lexer for possibly two-character tokens!"),
        }
    }

    pub fn get_keyword_type(value: &str) -> Option<TokenType> {
        match value {
            "and" => Some(TokenType::And),
            "class" => Some(TokenType::Class),
            "else" => Some(TokenType::Else),
            "false" => Some(TokenType::False),
            "for" => Some(TokenType::For),
            "fun" => Some(TokenType::Fun),
            "if" => Some(TokenType::If),
            "nil" => Some(TokenType::Nil),
            "or" => Some(TokenType::Or),
            "print" => Some(TokenType::Print),
            "return" => Some(TokenType::Return),
            "super" => Some(TokenType::Super),
            "this" => Some(TokenType::This),
            "true" => Some(TokenType::True),
            "var" => Some(TokenType::Var),
            "while" => Some(TokenType::While),
            _ => None,
        }
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
