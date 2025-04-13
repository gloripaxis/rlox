use std::{fmt, rc::Rc};

use super::{callable::LoxCallable, literal::Lit};

#[derive(Clone)]
pub enum Val {
    Nil,
    Num(f64),
    Str(Rc<str>),
    Bool(bool),
    Func(Rc<dyn LoxCallable>),
}

impl fmt::Debug for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Val::Nil => write!(f, "nil"),
            Val::Num(x) => write!(f, "number ({})", x),
            Val::Str(x) => write!(f, "string \"{}\"", x),
            Val::Bool(x) => write!(f, "boolean ({})", x),
            Val::Func(x) => write!(f, "function {}", x.name()),
        }
    }
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Val::Nil => write!(f, "nil"),
            Val::Num(x) => {
                if x.fract().abs() < f64::EPSILON {
                    write!(f, "{:.0}", x)
                } else {
                    write!(f, "{}", x)
                }
            }
            Val::Str(x) => write!(f, "{}", x),
            Val::Bool(x) => write!(f, "{}", x),
            Val::Func(x) => write!(f, "{}", x.name()),
        }
    }
}

impl PartialEq<Val> for Val {
    fn eq(&self, other: &Val) -> bool {
        match (self, other) {
            (Val::Nil, Val::Nil) => true,
            (Val::Str(x), Val::Str(y)) => x == y,
            (Val::Num(x), Val::Num(y)) => x == y,
            (Val::Bool(x), Val::Bool(y)) => x == y,
            _ => false,
        }
    }
}

impl Val {
    pub fn from_literal(literal: &Lit) -> Self {
        match literal {
            Lit::Bool(x) => Self::Bool(*x),
            Lit::Nil => Self::Nil,
            Lit::Num(x) => Self::Num(*x),
            Lit::Str(x) => Self::Str(Rc::clone(x)),
            Lit::Id(_) => unreachable!("Should never call `from_literal` with a Lit::Id variant!"),
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Val::Nil => false,
            Val::Bool(x) => *x,
            _ => true,
        }
    }
}
