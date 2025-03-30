use std::{collections::HashMap, error::Error};

use crate::{
    errors::{ErrorMessage, ErrorType, RloxError},
    lexer::token::{Literal, Token},
};

pub struct Environment {
    env: HashMap<String, Literal>,
}

impl Environment {
    pub fn new() -> Self {
        Self { env: HashMap::new() }
    }

    pub fn define(&mut self, name: String, value: Literal) {
        self.env.insert(name, value);
    }

    pub fn assign(&mut self, name: &Token, value: Literal) -> Result<(), Box<dyn Error>> {
        if !self.env.contains_key(name.get_lexeme()) {
            let (line, column) = name.get_location();
            return Err(Box::new(RloxError::new(vec![ErrorMessage::new(
                ErrorType::Runtime,
                format!("Undefined variable '{}'", name.get_lexeme()),
                line,
                column,
            )])));
        }

        self.env.insert(String::from(name.get_lexeme()), value);
        Ok(())
    }

    pub fn get(&self, token: &Token) -> Result<Literal, Box<dyn Error>> {
        if !self.env.contains_key(token.get_lexeme()) {
            let (line, column) = token.get_location();
            return Err(Box::new(RloxError::new(vec![ErrorMessage::new(
                ErrorType::Runtime,
                format!("Undefined variable: '{}'", token.get_lexeme()),
                line,
                column,
            )])));
        }
        Ok(self.env.get(token.get_lexeme()).unwrap().to_owned())
    }
}
