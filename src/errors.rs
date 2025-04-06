use std::error::Error;
use std::fmt;

use crate::types::token::Token;

#[derive(Debug)]
pub struct ErrorInfo {
    message: String,
    start: Option<(usize, usize)>,
    location: (usize, usize),
}

impl fmt::Display for ErrorInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.start {
            Some((line, col)) => write!(
                f,
                " between line {}, column {} and line {}, column {}: {}",
                line, col, self.location.0, self.location.1, self.message
            ),
            None => write!(
                f,
                " at line {}, column {}: {}",
                self.location.0, self.location.1, self.message
            ),
        }
    }
}

impl ErrorInfo {
    pub fn new(location: (usize, usize), message: String) -> Self {
        Self {
            message,
            start: None,
            location,
        }
    }

    pub fn with_start(start: (usize, usize), location: (usize, usize), message: String) -> Self {
        Self {
            message,
            start: Some(start),
            location,
        }
    }

    pub fn from_token(token: &Token, message: String) -> Self {
        Self {
            message,
            start: None,
            location: token.get_location(),
        }
    }
}

#[derive(Debug)]
pub enum LoxError {
    Lexer(Vec<ErrorInfo>),
    Syntax(Vec<ErrorInfo>),
    Runtime(ErrorInfo),
}

impl Error for LoxError {}

impl fmt::Display for LoxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LoxError::Lexer(errors) => {
                let emsg = errors
                    .iter()
                    .map(|err| format!("Lexer Error{}", err))
                    .collect::<Vec<_>>()
                    .join("\n");
                write!(f, "{}", emsg)
            }
            LoxError::Syntax(errors) => {
                let emsg = errors
                    .iter()
                    .map(|err| format!("Syntax Error{}", err))
                    .collect::<Vec<_>>()
                    .join("\n");
                write!(f, "{}", emsg)
            }
            LoxError::Runtime(error) => write!(f, "Runtime Error{}", error),
        }
    }
}
