use std::{cell::RefCell, fmt::Debug, rc::Rc};

use crate::{
    compile::{env::Environment, interpreter::Interpreter},
    errors::LoxError,
};

use super::{class::LoxInstance, statement::Stmt, token::Token, value::Val};

#[derive(Debug, Copy, Clone)]
pub enum FunctionType {
    None,
    Function,
    Method,
}

pub trait LoxCallable: Debug {
    fn arity(&self) -> usize;
    fn call(&self, interpreter: &mut Interpreter, args: Vec<Val>) -> Result<Val, LoxError>;
    fn name(&self) -> String;
}

#[derive(Debug)]
pub struct LoxFunction {
    name: Rc<Token>,
    params: Vec<Rc<Token>>,
    body: Vec<Rc<Stmt>>,
    closure: Rc<RefCell<Environment>>,
    is_init: bool,
}

impl LoxFunction {
    pub fn new(
        name: Rc<Token>,
        params: Vec<Rc<Token>>,
        body: Vec<Rc<Stmt>>,
        closure: Rc<RefCell<Environment>>,
        is_init: bool,
    ) -> Self {
        Self {
            name,
            params,
            body,
            closure,
            is_init,
        }
    }

    pub fn bind(&self, instance: Rc<RefCell<LoxInstance>>) -> Self {
        let mut env = Environment::new(Some(Rc::clone(&self.closure)));
        env.define(String::from("this"), Val::Instance(Rc::clone(&instance)));
        Self {
            name: Rc::clone(&self.name),
            params: self.params.iter().map(Rc::clone).collect(),
            body: self.body.iter().map(Rc::clone).collect(),
            closure: Rc::new(RefCell::new(env)),
            is_init: self.is_init,
        }
    }
}

impl LoxCallable for LoxFunction {
    fn arity(&self) -> usize {
        self.params.len()
    }

    fn call(&self, interpreter: &mut Interpreter, args: Vec<Val>) -> Result<Val, LoxError> {
        let mut env = Environment::new(Some(Rc::clone(&self.closure)));
        for (param, arg) in self.params.iter().zip(args) {
            env.define(param.get_lexeme(), arg);
        }
        let result = interpreter.execute_block(&self.body, env);
        if !self.is_init {
            match result {
                Ok(_) => Ok(Val::Nil),
                Err(x) => match x {
                    LoxError::Return(val) => Ok(val),
                    e => Err(e),
                },
            }
        } else {
            let init_val = self.closure.borrow().get_here("this");
            match result {
                Ok(_) => Ok(init_val),
                Err(x) => match x {
                    LoxError::Return(_) => Ok(init_val),
                    e => Err(e),
                },
            }
        }
    }

    fn name(&self) -> String {
        format!("<fn::{}", self.name.get_lexeme())
    }
}
