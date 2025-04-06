use super::expr::Expression;
use crate::{errors::LoxError, types::token::Token, visitors::Visitor};

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(Expression),
    Print(Expression),
    Var(Token, Option<Expression>),
    Block(Vec<Statement>),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    While(Expression, Box<Statement>),
}

impl Statement {
    pub fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> Result<(), LoxError> {
        match self {
            Statement::Print(expr) => visitor.visit_print_stmt(expr),
            Statement::Expression(expr) => visitor.visit_expression_stmt(expr),
            Statement::Var(token, initializer) => visitor.visit_var_stmt(token, initializer),
            Statement::Block(stmts) => visitor.visit_block_stmt(stmts),
            Statement::If(cond, br_then, br_else) => visitor.visit_if_stmt(cond, br_then, br_else),
            Statement::While(cond, stmt) => visitor.visit_while_stmt(cond, stmt),
        }
    }
}
