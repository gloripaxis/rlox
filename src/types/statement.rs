use std::rc::Rc;

use super::expression::Expr;
use crate::{errors::LoxError, types::token::Token, visitors::Visitor};

#[derive(Debug, Clone)]
pub enum Stmt {
    Expression(Expr),
    Print(Expr),
    Var(Rc<Token>, Option<Expr>),
    Block(Vec<Stmt>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>),
}

impl Stmt {
    pub fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> Result<(), LoxError> {
        match self {
            Stmt::Print(expr) => visitor.visit_print_stmt(expr),
            Stmt::Expression(expr) => visitor.visit_expression_stmt(expr),
            Stmt::Var(token, initializer) => visitor.visit_var_stmt(token, initializer),
            Stmt::Block(stmts) => visitor.visit_block_stmt(stmts),
            Stmt::If(cond, br_then, br_else) => visitor.visit_if_stmt(cond, br_then, br_else),
            Stmt::While(cond, stmt) => visitor.visit_while_stmt(cond, stmt),
        }
    }
}
