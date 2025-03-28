use std::error::Error;
use std::fmt;


#[derive(Debug, Clone)]
pub enum ErrorType {
    LexerError
}

impl fmt::Display for ErrorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}


#[derive(Debug, Clone)]
pub struct ErrorMessage {
    error_type: ErrorType,
    message: String,
    line: usize
}

impl ErrorMessage {
    pub fn new(error_type: ErrorType, message: String, line: usize) -> Self {
        Self {
            error_type, message, line
        }
    }
}

impl fmt::Display for ErrorMessage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} at line {}: {}", self.error_type, self.line, self.message)
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
        let error_message = self.messages.iter()
            .map(|err| err.to_string())
            .collect::<Vec<_>>()
            .join("\n");

        write!(f, "{}", error_message)
    }
}