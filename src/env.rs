use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    errors::{ErrorInfo, LoxError},
    lexer::token::{Literal, Token},
};

#[derive(Debug, Clone)]
pub struct Environment {
    env: HashMap<String, Literal>,
    parent: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new(parent: Option<Rc<RefCell<Environment>>>) -> Self {
        Self {
            env: HashMap::new(),
            parent: parent.map(|x| Rc::clone(&x)),
        }
    }

    pub fn define(&mut self, name: String, value: Literal) {
        self.env.insert(name, value);
    }

    pub fn assign(&mut self, name: &Token, value: Literal) -> Result<(), LoxError> {
        if self.env.contains_key(name.get_lexeme()) {
            self.env.insert(String::from(name.get_lexeme()), value);
            return Ok(());
        }

        if let Some(env) = &self.parent {
            return env.borrow_mut().assign(name, value);
        }

        let message = format!("Undefined variable: '{}'", name.get_lexeme());
        Err(LoxError::Runtime(ErrorInfo::from_token(name, message)))
    }

    pub fn get(&self, name: &Token) -> Result<Literal, LoxError> {
        if self.env.contains_key(name.get_lexeme()) {
            return Ok(self.env.get(name.get_lexeme()).unwrap().to_owned());
        }

        if let Some(env) = &self.parent {
            return env.borrow_mut().get(name);
        }

        let message = format!("Undefined variable: '{}'", name.get_lexeme());
        Err(LoxError::Runtime(ErrorInfo::from_token(name, message)))
    }
}
