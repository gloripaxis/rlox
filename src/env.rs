use std::{cell::RefCell, collections::HashMap, error::Error, rc::Rc};

use crate::{
    errors::{ErrorMessage, ErrorType, RloxError},
    lexer::token::{Literal, Token},
};

#[derive(Debug, Clone)]
pub struct Environment {
    env: HashMap<String, Literal>,
    parent: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new(parent: Option<Rc<RefCell<Environment>>>) -> Self {
        match parent {
            None => Self {
                env: HashMap::new(),
                parent: None,
            },
            Some(env) => Self {
                env: HashMap::new(),
                parent: Some(Rc::clone(&env)),
            },
        }
    }

    pub fn define(&mut self, name: String, value: Literal) {
        self.env.insert(name, value);
    }

    pub fn assign(&mut self, name: &Token, value: Literal) -> Result<(), Box<dyn Error>> {
        if self.env.contains_key(name.get_lexeme()) {
            self.env.insert(String::from(name.get_lexeme()), value);
            return Ok(());
        }

        if let Some(env) = &self.parent {
            return env.borrow_mut().assign(name, value);
        }

        let (line, column) = name.get_location();
        Err(Box::new(RloxError::new(vec![ErrorMessage::new(
            ErrorType::Runtime,
            format!("Undefined variable '{}'", name.get_lexeme()),
            line,
            column,
        )])))
    }

    pub fn get(&self, token: &Token) -> Result<Literal, Box<dyn Error>> {
        if self.env.contains_key(token.get_lexeme()) {
            return Ok(self.env.get(token.get_lexeme()).unwrap().to_owned());
        }

        if let Some(env) = &self.parent {
            return env.borrow_mut().get(token);
        }

        let (line, column) = token.get_location();
        Err(Box::new(RloxError::new(vec![ErrorMessage::new(
            ErrorType::Runtime,
            format!("Undefined variable: '{}'", token.get_lexeme()),
            line,
            column,
        )])))
    }
}
