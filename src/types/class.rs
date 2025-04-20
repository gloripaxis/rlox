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
    superclass: Option<Rc<LoxClass>>,
    methods: HashMap<String, Rc<LoxFunction>>,
}

impl LoxClass {
    pub fn new(name: String, superclass: Option<Rc<LoxClass>>, methods: HashMap<String, Rc<LoxFunction>>) -> Self {
        Self {
            name: Rc::from(name),
            superclass,
            methods,
        }
    }

    pub fn find_method(&self, name: &str) -> Option<&Rc<LoxFunction>> {
        let mut method = self.methods.get(name);
        if let (None, Some(sc)) = (method, &self.superclass) {
            method = sc.find_method(name);
        }
        method
    }

    pub fn get_name(&self) -> String {
        self.name.to_string()
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

    pub fn get(&self, name: Rc<Token>, this: Rc<RefCell<LoxInstance>>) -> Result<Val, LoxError> {
        let lex = name.get_lexeme();
        if self.state.contains_key(&lex) {
            return Ok(self.state.get(&lex).unwrap().clone());
        }

        if let Some(x) = self.klass.find_method(&lex) {
            let method = Rc::clone(x);
            let bound_method = method.bind(this);
            return Ok(Val::Func(Rc::new(bound_method)));
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
        let initializer = self.find_method("init");
        if let Some(init) = initializer {
            return 1 + init.arity();
        }
        1
    }

    fn call(&self, interpreter: &mut Interpreter, args: Vec<Val>) -> Result<Val, LoxError> {
        let val = args.first().unwrap(); // arity checked before call, therefore must be correct
        if let Val::Class(klass) = val {
            let instance = LoxInstance::new(Rc::clone(klass));
            let instance = Rc::new(RefCell::new(instance));
            let initializer = klass.find_method("init");

            if let Some(init) = initializer {
                let new_args: Vec<Val> = args[1..].to_vec();
                init.bind(Rc::clone(&instance)).call(interpreter, new_args)?;
            }
            return Ok(Val::Instance(instance));
        }
        unreachable!("LoxClass can only be called with itself as the first argument");
    }

    fn name(&self) -> String {
        format!("{self}")
    }
}
