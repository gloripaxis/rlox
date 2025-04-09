use std::{io, rc::Rc};

use crate::{
    compile::interpreter::Interpreter,
    errors::LoxError,
    types::{callable::LoxCallable, value::Val},
};

#[derive(Debug)]
pub struct ReadFunction {}

impl LoxCallable for ReadFunction {
    fn arity(&self) -> usize {
        0
    }

    fn call(&self, _: &mut Interpreter, _: Vec<Val>) -> Result<Val, LoxError> {
        let mut input = String::from("");
        io::stdin().read_line(&mut input).unwrap();
        Ok(Val::Str(Rc::from((input[..input.len() - 1]).to_string())))
    }

    fn name(&self) -> String {
        String::from("<builtin_fn::read>")
    }
}

impl ReadFunction {
    pub fn new() -> Self {
        Self {}
    }
}
