use std::{fmt, rc::Rc};

use crate::{compile::interpreter::Interpreter, errors::LoxError};

use super::{callable::LoxCallable, value::Val};

#[derive(Debug, Clone)]
pub struct LoxClass {
    name: Rc<str>,
}

impl LoxClass {
    pub fn new(name: String) -> Self {
        Self { name: Rc::from(name) }
    }
}

impl fmt::Display for LoxClass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

pub struct LoxInstance {
    klass: Rc<LoxClass>,
}

impl LoxInstance {
    pub fn new(klass: Rc<LoxClass>) -> Self {
        Self { klass }
    }
}

impl fmt::Display for LoxInstance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} instance", self.klass)
    }
}

impl LoxCallable for LoxClass {
    fn arity(&self) -> usize {
        1
    }

    fn call(&self, _interpreter: &mut Interpreter, _args: Vec<Val>) -> Result<Val, LoxError> {
        let val = _args.first().unwrap(); // arity checked before call, therefore must be correct
        if let Val::Class(klass) = val {
            let instance = LoxInstance::new(Rc::clone(klass));
            return Ok(Val::Instance(Rc::new(instance)));
        }
        unreachable!("LoxClass can only be called with itself as the first argument");
    }

    fn name(&self) -> String {
        format!("{self}")
    }
}
