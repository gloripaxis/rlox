use std::fmt::Debug;

use crate::compile::interpreter::Interpreter;

use super::value::Val;

pub trait LoxCallable: Debug {
    fn arity(&self) -> usize;
    fn call(&self, interpreter: &Interpreter, args: Vec<Val>) -> Val;
    fn name(&self) -> String;
}
