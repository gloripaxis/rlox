use std::time::{SystemTime, UNIX_EPOCH};

use crate::{
    compile::interpreter::Interpreter,
    errors::LoxError,
    types::{callable::LoxCallable, value::Val},
};

#[derive(Debug)]
pub struct ClockFunction {}

impl LoxCallable for ClockFunction {
    fn arity(&self) -> usize {
        0
    }

    fn call(&self, _: &mut Interpreter, _: Vec<Val>) -> Result<Val, LoxError> {
        Ok(Val::Num(
            SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_secs_f64(),
        ))
    }

    fn name(&self) -> String {
        String::from("<builtin_fn::clock>")
    }
}

impl ClockFunction {
    pub fn new() -> Self {
        Self {}
    }
}
