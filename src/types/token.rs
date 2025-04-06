use std::fmt;

use super::{literal::Lit, position::Pos};

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
        match self {
            // Keywords should be printed lowercase
            TokenType::And
            | TokenType::Class
            | TokenType::Else
            | TokenType::False
            | TokenType::For
            | TokenType::Fun
            | TokenType::If
            | TokenType::Nil
            | TokenType::Or
            | TokenType::Print
            | TokenType::Return
            | TokenType::Super
            | TokenType::This
            | TokenType::True
            | TokenType::Var
            | TokenType::While => {
                let s = format!("{:?}", self).to_lowercase();
                write!(f, "{}", s)
            }
            // EndOfFile is a special token
            TokenType::EndOfFile => {
                write!(f, "EOF")
            }
            // All operators and special symbols should be represented truthfully
            TokenType::Assign => write!(f, "="),
            TokenType::Bang => write!(f, "!"),
            TokenType::Comma => write!(f, ","),
            TokenType::Dot => write!(f, "."),
            TokenType::Eq => write!(f, "=="),
            TokenType::Geq => write!(f, ">="),
            TokenType::Gt => write!(f, ">"),
            TokenType::LeftBrace => write!(f, "{{"),
            TokenType::LeftParen => write!(f, "("),
            TokenType::Leq => write!(f, "<="),
            TokenType::Lt => write!(f, "<"),
            TokenType::Minus => write!(f, "-"),
            TokenType::Neq => write!(f, "!="),
            TokenType::Plus => write!(f, "+"),
            TokenType::RightBrace => write!(f, "}}"),
            TokenType::RightParen => write!(f, ")"),
            TokenType::Semicolon => write!(f, ";"),
            TokenType::Slash => write!(f, "/"),
            TokenType::Star => write!(f, "*"),
            // String, Number and Identifier are represented by the enum name
            _ => write!(f, "{:?}", self),
        }
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

#[derive(Clone)]
pub struct Token {
    ttype: TokenType,
    literal: Lit,
    position: Pos,
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "[{:?}] ({}:{}) '{}'",
            self.ttype,
            self.position.line,
            self.position.col,
            self.get_lexeme()
        )
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.get_lexeme())
    }
}

impl Token {
    pub fn new(ttype: TokenType, literal: Lit, position: Pos) -> Self {
        Self {
            ttype,
            literal,
            position,
        }
    }

    pub fn get_type(&self) -> TokenType {
        self.ttype
    }

    pub fn get_literal(&self) -> Lit {
        self.literal.clone()
    }

    pub fn get_lexeme(&self) -> String {
        match self.literal {
            Lit::Nil => format!("{}", self.ttype),
            _ => format!("{}", self.literal),
        }
    }

    pub fn get_position(&self) -> Pos {
        self.position
    }
}
