use std::{cell::RefCell, collections::HashMap, fmt, rc::Rc};

use crate::{compile::interpreter::Interpreter, errors::LoxError};

use super::{
    callable::{LoxCallable, LoxFunction},
    token::Token,
    value::Val,
};

#[derive(Debug)]
pub struct LoxClass {
    name: Rc<str>,
    methods: HashMap<String, Rc<LoxFunction>>,
}

impl LoxClass {
    pub fn new(name: String, methods: HashMap<String, Rc<LoxFunction>>) -> Self {
        Self {
            name: Rc::from(name),
            methods,
        }
    }

    pub fn find_method(&self, name: &str) -> Option<&Rc<LoxFunction>> {
        self.methods.get(name)
    }
}

impl fmt::Display for LoxClass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

pub struct LoxInstance {
    klass: Rc<LoxClass>,
    state: HashMap<String, Val>,
}

impl LoxInstance {
    pub fn new(klass: Rc<LoxClass>) -> Self {
        Self {
            klass,
            state: HashMap::new(),
        }
    }

    pub fn get(&self, name: Rc<Token>) -> Result<Val, LoxError> {
        let lex = name.get_lexeme();
        if self.state.contains_key(&lex) {
            return Ok(self.state.get(&lex).unwrap().clone());
        }

        if let Some(x) = self.klass.find_method(&lex) {
            let method = Rc::clone(x);
            return Ok(Val::Func(method));
        }

        Err(LoxError::undefined_property(
            name.get_position(),
            &format!("{self}"),
            &name.get_literal(),
        ))
    }

    pub fn set(&mut self, name: Rc<Token>, value: Val) {
        self.state.insert(name.get_lexeme(), value);
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
            return Ok(Val::Instance(Rc::new(RefCell::new(instance))));
        }
        unreachable!("LoxClass can only be called with itself as the first argument");
    }

    fn name(&self) -> String {
        format!("{self}")
    }
}
