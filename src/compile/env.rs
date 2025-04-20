use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    builtins::{clock::ClockFunction, read::ReadFunction},
    types::value::Val,
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

    #[allow(clippy::map_entry)]
    pub fn assign(&mut self, name: String, value: Val) -> Result<(), ()> {
        if self.env.contains_key(&name) {
            self.env.insert(name, value);
            Ok(())
        } else if let Some(env) = &self.parent {
            env.borrow_mut().assign(name, value)
        } else {
            Err(())
        }
    }

    pub fn get(&self, name: &str) -> Result<Val, ()> {
        if self.env.contains_key(name) {
            return Ok(self.env.get(name).unwrap().to_owned());
        }

        if let Some(env) = &self.parent {
            return env.borrow_mut().get(name);
        }
        Err(())
    }

    pub fn get_here(&self, name: &str) -> Val {
        self.env.get(name).unwrap().to_owned()
    }

    pub fn assign_here(&mut self, name: String, value: Val) {
        self.env.insert(name, value);
    }

    pub fn get_parent(&self) -> Option<Rc<RefCell<Environment>>> {
        if let Some(x) = &self.parent {
            return Some(Rc::clone(x));
        }
        None
    }
}
