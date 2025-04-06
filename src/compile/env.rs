use std::{
    cell::RefCell,
    collections::{HashMap, hash_map::Entry},
    rc::Rc,
};

use crate::{
    errors::LoxError,
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

        Err(LoxError::undefined_variable(name.get_position(), &name.get_literal()))
    }

    pub fn get(&self, name: &Token) -> Result<Lit, LoxError> {
        if self.env.contains_key(&name.get_lexeme()) {
            return Ok(self.env.get(&name.get_lexeme()).unwrap().to_owned());
        }

        if let Some(env) = &self.parent {
            return env.borrow_mut().get(name);
        }

        Err(LoxError::undefined_variable(name.get_position(), &name.get_literal()))
    }
}
