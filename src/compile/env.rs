use std::{
    cell::RefCell,
    collections::{HashMap, hash_map::Entry},
    rc::Rc,
};

use crate::{
    builtins::{clock::ClockFunction, read::ReadFunction},
    errors::LoxError,
    types::{token::Token, value::Val},
};

#[derive(Debug, Clone)]
pub struct Environment {
    env: HashMap<String, Val>,
    parent: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn global() -> Self {
        let mut env = Self::new(None);
        env.define(String::from("clock"), Val::Func(Rc::new(ClockFunction::new())));
        env.define(String::from("read"), Val::Func(Rc::new(ReadFunction::new())));
        env
    }

    pub fn new(parent: Option<Rc<RefCell<Environment>>>) -> Self {
        Self {
            env: HashMap::new(),
            parent: parent.map(|x| Rc::clone(&x)),
        }
    }

    pub fn define(&mut self, name: String, value: Val) {
        self.env.insert(name, value);
    }

    pub fn assign(&mut self, name: &Token, value: Val) -> Result<(), LoxError> {
        if let Entry::Occupied(mut e) = self.env.entry(name.get_lexeme()) {
            e.insert(value);
            return Ok(());
        }

        if let Some(env) = &self.parent {
            return env.borrow_mut().assign(name, value);
        }

        Err(LoxError::undefined_variable(name.get_position(), &name.get_literal()))
    }

    pub fn get(&self, name: &Token) -> Result<Val, LoxError> {
        if self.env.contains_key(&name.get_lexeme()) {
            return Ok(self.env.get(&name.get_lexeme()).unwrap().to_owned());
        }

        if let Some(env) = &self.parent {
            return env.borrow_mut().get(name);
        }

        Err(LoxError::undefined_variable(name.get_position(), &name.get_literal()))
    }

    pub fn get_here(&self, name: &Token) -> Result<Val, LoxError> {
        Ok(self.env.get(&name.get_lexeme()).unwrap().to_owned())
    }

    pub fn assign_here(&mut self, name: &Token, value: Val) -> Result<(), LoxError> {
        self.env.insert(name.get_lexeme(), value);
        Ok(())
    }

    pub fn get_parent(&self) -> Option<Rc<RefCell<Environment>>> {
        if let Some(x) = &self.parent {
            return Some(Rc::clone(x));
        }
        None
    }
}
