use std::{fmt, rc::Rc};

#[derive(Clone)]
pub enum Lit {
    Nil,
    Num(f64),
    Str(Rc<str>),
    Bool(bool),
    Id(Rc<str>),
}

impl fmt::Debug for Lit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Lit::Nil => write!(f, "nil"),
            Lit::Num(x) => write!(f, "number ({})", x),
            Lit::Str(x) => write!(f, "string \"{}\"", x),
            Lit::Bool(x) => write!(f, "boolean ({})", x),
            Lit::Id(x) => write!(f, "identifier {}", x),
        }
    }
}

impl fmt::Display for Lit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Lit::Nil => write!(f, "nil"),
            Lit::Num(x) => {
                if x.fract().abs() < f64::EPSILON {
                    write!(f, "{:.0}", x)
                } else {
                    write!(f, "{}", x)
                }
            }
            Lit::Str(x) => write!(f, "\"{}\"", x),
            Lit::Bool(x) => write!(f, "{}", x),
            Lit::Id(x) => write!(f, "{}", x),
        }
    }
}

impl PartialEq<Lit> for Lit {
    fn eq(&self, other: &Lit) -> bool {
        match (self, other) {
            (Lit::Nil, Lit::Nil) => true,
            (Lit::Str(x), Lit::Str(y)) => x == y,
            (Lit::Num(x), Lit::Num(y)) => x == y,
            (Lit::Bool(x), Lit::Bool(y)) => x == y,
            _ => false,
        }
    }
}

impl Lit {
    pub fn is_truthy(&self) -> bool {
        match self {
            Lit::Nil => false,
            Lit::Bool(x) => *x,
            _ => true,
        }
    }
}
