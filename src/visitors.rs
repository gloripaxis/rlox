use std::rc::Rc;

use crate::{
    errors::LoxError,
    types::{expression::Expr, statement::Stmt, token::Token},
};

pub trait Visitor<T> {
    fn visit_unary_expr(&mut self, op: Rc<Token>, right: Rc<Expr>) -> Result<T, LoxError>;
    fn visit_binary_expr(&mut self, left: Rc<Expr>, op: Rc<Token>, right: Rc<Expr>) -> Result<T, LoxError>;
    fn visit_logic_expr(&mut self, left: Rc<Expr>, op: Rc<Token>, right: Rc<Expr>) -> Result<T, LoxError>;
    fn visit_grouping_expr(&mut self, expr: Rc<Expr>) -> Result<T, LoxError>;
    fn visit_literal_expr(&mut self, value: Rc<Token>) -> Result<T, LoxError>;
    fn visit_variable_expr(&mut self, name: Rc<Token>) -> Result<T, LoxError>;
    fn visit_assign_expr(&mut self, name: Rc<Token>, right: Rc<Expr>) -> Result<T, LoxError>;
    fn visit_call_expr(&mut self, callee: Rc<Expr>, paren: Rc<Token>, args: &[Rc<Expr>]) -> Result<T, LoxError>;
    fn visit_get_expr(&mut self, object: Rc<Expr>, name: Rc<Token>) -> Result<T, LoxError>;
    fn visit_set_expr(&mut self, object: Rc<Expr>, name: Rc<Token>, value: Rc<Expr>) -> Result<T, LoxError>;
    fn visit_this_expr(&mut self, token: Rc<Token>) -> Result<T, LoxError>;
    fn visit_super_expr(&mut self, keyword: Rc<Token>, method: Rc<Token>) -> Result<T, LoxError>;

    fn visit_expression_stmt(&mut self, expr: Rc<Expr>) -> Result<(), LoxError>;
    fn visit_print_stmt(&mut self, expr: Rc<Expr>) -> Result<(), LoxError>;
    fn visit_var_stmt(&mut self, name: Rc<Token>, init: &Option<Rc<Expr>>) -> Result<(), LoxError>;
    fn visit_block_stmt(&mut self, statements: &[Rc<Stmt>]) -> Result<(), LoxError>;
    fn visit_if_stmt(&mut self, cond: Rc<Expr>, b_then: Rc<Stmt>, b_else: &Option<Rc<Stmt>>) -> Result<(), LoxError>;
    fn visit_while_stmt(&mut self, cond: Rc<Expr>, stmt: Rc<Stmt>) -> Result<(), LoxError>;
    fn visit_function_stmt(&mut self, name: Rc<Token>, params: &[Rc<Token>], body: &[Rc<Stmt>])
    -> Result<(), LoxError>;
    fn visit_return_stmt(&mut self, token: Rc<Token>, expr: &Option<Rc<Expr>>) -> Result<(), LoxError>;
    fn visit_class_stmt(
        &mut self,
        name: Rc<Token>,
        superclass: &Option<Rc<Expr>>,
        methods: &[Rc<Stmt>],
    ) -> Result<(), LoxError>;
}
