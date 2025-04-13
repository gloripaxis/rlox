use std::{fmt::Debug, rc::Rc};

use crate::{
    compile::{env::Environment, interpreter::Interpreter},
    errors::LoxError,
};

use super::{statement::Stmt, token::Token, value::Val};

pub trait LoxCallable: Debug {
    fn arity(&self) -> usize;
    fn call(&self, interpreter: &mut Interpreter, args: Vec<Val>) -> Result<Val, LoxError>;
    fn name(&self) -> String;
}

#[derive(Debug)]
pub struct LoxFunction {
    name: Rc<Token>,
    params: Vec<Rc<Token>>,
    body: Vec<Stmt>,
}

impl LoxFunction {
    pub fn new(name: Rc<Token>, params: Vec<Rc<Token>>, body: Vec<Stmt>) -> Self {
        Self { name, params, body }
    }
}

impl LoxCallable for LoxFunction {
    fn arity(&self) -> usize {
        self.params.len()
    }

    fn call(&self, interpreter: &mut Interpreter, args: Vec<Val>) -> Result<Val, LoxError> {
        let mut env = Environment::new(Some(interpreter.get_global_env()));
        for (param, arg) in self.params.iter().zip(args) {
            env.define(param.get_lexeme(), arg);
        }
        interpreter.execute_block(&self.body, env)?;
        Ok(Val::Nil)
    }

    fn name(&self) -> String {
        format!("<fn::{}", self.name.get_lexeme())
    }
}
