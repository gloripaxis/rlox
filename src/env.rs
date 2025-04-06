use std::{
    cell::RefCell,
    collections::{HashMap, hash_map::Entry},
    rc::Rc,
};

use crate::{
    errors::{ErrorInfo, LoxError},
    types::{literal::Lit, token::Token},
};

#[derive(Debug, Clone)]
pub struct Environment {
    env: HashMap<String, Lit>,
    parent: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new(parent: Option<Rc<RefCell<Environment>>>) -> Self {
        Self {
            env: HashMap::new(),
            parent: parent.map(|x| Rc::clone(&x)),
        }
    }

    pub fn define(&mut self, name: String, value: Lit) {
        self.env.insert(name, value);
    }

    pub fn assign(&mut self, name: &Token, value: Lit) -> Result<(), LoxError> {
        if let Entry::Occupied(mut e) = self.env.entry(name.get_lexeme()) {
            e.insert(value);
            return Ok(());
        }

        if let Some(env) = &self.parent {
            return env.borrow_mut().assign(name, value);
        }

        let message = format!("Undefined variable: '{}'", name.get_lexeme());
        Err(LoxError::Runtime(ErrorInfo::from_token(name, message)))
    }

    pub fn get(&self, name: &Token) -> Result<Lit, LoxError> {
        if self.env.contains_key(&name.get_lexeme()) {
            return Ok(self.env.get(&name.get_lexeme()).unwrap().to_owned());
        }

        if let Some(env) = &self.parent {
            return env.borrow_mut().get(name);
        }

        let message = format!("Undefined variable: '{}'", name.get_lexeme());
        Err(LoxError::Runtime(ErrorInfo::from_token(name, message)))
    }
}
