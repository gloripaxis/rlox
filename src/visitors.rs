use std::rc::Rc;

use crate::{
    errors::LoxError,
    types::{expression::Expr, statement::Stmt, token::Token},
};

pub trait Visitor<T> {
    fn visit_unary_expr(&mut self, op: &Token, right: &Expr) -> Result<T, LoxError>;
    fn visit_binary_expr(&mut self, left: &Expr, op: &Token, right: &Expr) -> Result<T, LoxError>;
    fn visit_logic_expr(&mut self, left: &Expr, op: &Token, right: &Expr) -> Result<T, LoxError>;
    fn visit_grouping_expr(&mut self, expr: &Expr) -> Result<T, LoxError>;
    fn visit_literal_expr(&mut self, value: &Token) -> Result<T, LoxError>;
    fn visit_variable_expr(&mut self, name: &Token) -> Result<T, LoxError>;
    fn visit_assign_expr(&mut self, name: &Token, right: &Expr) -> Result<T, LoxError>;
    fn visit_call_expr(&mut self, callee: &Expr, paren: &Token, args: &[Expr]) -> Result<T, LoxError>;

    fn visit_expression_stmt(&mut self, expr: &Expr) -> Result<(), LoxError>;
    fn visit_print_stmt(&mut self, expr: &Expr) -> Result<(), LoxError>;
    fn visit_var_stmt(&mut self, name: &Token, init: &Option<Rc<Expr>>) -> Result<(), LoxError>;
    fn visit_block_stmt(&mut self, statements: &[Rc<Stmt>]) -> Result<(), LoxError>;
    fn visit_if_stmt(&mut self, cond: &Expr, b_then: &Stmt, b_else: &Option<Rc<Stmt>>) -> Result<(), LoxError>;
    fn visit_while_stmt(&mut self, cond: &Expr, stmt: &Stmt) -> Result<(), LoxError>;
    fn visit_function_stmt(&mut self, name: Rc<Token>, params: &[Rc<Token>], body: &[Rc<Stmt>])
    -> Result<(), LoxError>;
    fn visit_return_stmt(&mut self, token: Rc<Token>, expr: &Option<Rc<Expr>>) -> Result<(), LoxError>;
}
