use std::rc::Rc;

use super::expression::Expr;
use crate::{errors::LoxError, types::token::Token, visitors::Visitor};

#[derive(Debug, Clone)]
pub enum Stmt {
    Expression(Rc<Expr>),
    Print(Rc<Expr>),
    Var(Rc<Token>, Option<Rc<Expr>>),
    Block(Vec<Rc<Stmt>>),
    If(Rc<Expr>, Rc<Stmt>, Option<Rc<Stmt>>),
    While(Rc<Expr>, Rc<Stmt>),
    Function(Rc<Token>, Vec<Rc<Token>>, Vec<Rc<Stmt>>),
    Return(Rc<Token>, Option<Rc<Expr>>),
}

impl Stmt {
    pub fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> Result<(), LoxError> {
        match self {
            Stmt::Print(expr) => visitor.visit_print_stmt(Rc::clone(expr)),
            Stmt::Expression(expr) => visitor.visit_expression_stmt(Rc::clone(expr)),
            Stmt::Var(token, initializer) => visitor.visit_var_stmt(Rc::clone(token), initializer),
            Stmt::Block(stmts) => visitor.visit_block_stmt(stmts),
            Stmt::If(cond, br_then, br_else) => visitor.visit_if_stmt(Rc::clone(cond), Rc::clone(br_then), br_else),
            Stmt::While(cond, stmt) => visitor.visit_while_stmt(Rc::clone(cond), Rc::clone(stmt)),
            Stmt::Function(name, params, body) => visitor.visit_function_stmt(Rc::clone(name), params, body),
            Stmt::Return(token, expr) => visitor.visit_return_stmt(Rc::clone(token), expr),
        }
    }
}
