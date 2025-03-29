use std::error::Error;
use std::fmt;

#[derive(Debug, Clone)]
pub enum ErrorType {
    Lexer,
    Syntax,
    Runtime,
}

impl fmt::Display for ErrorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}Error", self)
    }
}

#[derive(Debug, Clone)]
pub struct ErrorMessage {
    error_type: ErrorType,
    message: String,
    line: usize,
    column: usize,
}

impl ErrorMessage {
    pub fn new(error_type: ErrorType, message: String, line: usize, column: usize) -> Self {
        Self {
            error_type,
            message,
            line,
            column,
        }
    }
}

impl fmt::Display for ErrorMessage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} at line {}, column {}: {}",
            self.error_type, self.line, self.column, self.message
        )
    }
}

#[derive(Debug)]
pub struct RloxError {
    messages: Vec<ErrorMessage>,
}

impl Error for RloxError {}

impl RloxError {
    pub fn new(messages: Vec<ErrorMessage>) -> Self {
        Self { messages }
    }
}

impl fmt::Display for RloxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let error_message = self
            .messages
            .iter()
            .map(|err| err.to_string())
            .collect::<Vec<_>>()
            .join("\n");

        write!(f, "{}", error_message)
    }
}
