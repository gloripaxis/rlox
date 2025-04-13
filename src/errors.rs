use std::error::Error;
use std::fmt;

use crate::types::expression::Expr;
use crate::types::literal::Lit;
use crate::types::position::Pos;
use crate::types::token::TokenType;
use crate::types::value::Val;

#[derive(Debug)]
pub enum LoxError {
    Lexer(Pos, String),
    Syntax(Pos, String),
    Runtime(Pos, String),
    Return(Val),
}

impl Error for LoxError {}

impl fmt::Display for LoxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LoxError::Lexer(pos, msg) => write!(f, "ParseError at {pos}: {msg}"),
            LoxError::Syntax(pos, msg) => write!(f, "SyntaxError at {pos}: {msg}"),
            LoxError::Runtime(pos, msg) => write!(f, "Runtime Error at {pos}: {msg}"),
            LoxError::Return(val) => write!(f, "{}", val),
        }
    }
}

impl LoxError {
    // ParseErrors (Lexer)
    pub fn illegal_character(pos: Pos, c: char) -> Self {
        Self::Lexer(pos, format!("Encountered illegal character '{c}'"))
    }

    pub fn unterminated_string(pos: Pos, start: Pos) -> Self {
        Self::Lexer(pos, format!("Unterminated string starting at {start}"))
    }

    pub fn invalid_escape(pos: Pos, c: char) -> Self {
        Self::Lexer(pos, format!("Invalid escape sequence '\\{c}'"))
    }

    // SyntaxErrors (Parser)
    pub fn expected(pos: Pos, exp: &str, after: &str, at: &str) -> Self {
        Self::Syntax(pos, format!("Expected '{}' after {} at {}", exp, after, at))
    }

    pub fn invalid_assignment(pos: Pos, expr: &Expr) -> Self {
        Self::Syntax(pos, format!("Invalid assignment target: '{expr}'"))
    }

    pub fn too_many_args(pos: Pos) -> Self {
        Self::Syntax(pos, String::from("Function received more than 255 arguments"))
    }

    pub fn too_many_params(pos: Pos) -> Self {
        Self::Syntax(pos, String::from("Function defined with more than 255 parameters"))
    }

    // RuntimeErrors (Interpreter)
    pub fn unary_operand(pos: Pos, op: TokenType, value: &Val) -> Self {
        Self::Runtime(pos, format!("Operand of '{}' must be a number; found {:?}", op, value))
    }

    pub fn binary_operands(pos: Pos, op: TokenType, left: &Val, right: &Val) -> Self {
        Self::Runtime(
            pos,
            format!("Operands of '{}' must be numbers; found {:?} and {:?}", op, left, right),
        )
    }

    pub fn plus_operands(pos: Pos, left: &Val, right: &Val) -> Self {
        Self::Runtime(
            pos,
            format!(
                "Operands of '+' must both be strings or numbers; found {:?} and {:?}",
                left, right
            ),
        )
    }

    pub fn undefined_variable(pos: Pos, name: &Lit) -> Self {
        Self::Runtime(pos, format!("Undefined variable '{}'", name))
    }

    pub fn not_callable(pos: Pos, callee: &Val) -> Self {
        Self::Runtime(pos, format!("Can only call functions and classes; found {callee}"))
    }

    pub fn wrong_arity(pos: Pos, callee: &str, expected: usize, received: usize) -> Self {
        Self::Runtime(
            pos,
            format!("Function {callee} expected {expected} arguments but got {received}"),
        )
    }
}
