use std::fmt::Debug;

use crate::{compile::interpreter::Interpreter, errors::LoxError};

use super::value::Val;

pub trait LoxCallable: Debug {
    fn arity(&self) -> usize;
    fn call(&self, interpreter: &mut Interpreter, args: Vec<Val>) -> Result<Val, LoxError>;
    fn name(&self) -> String;
}
