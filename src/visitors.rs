use crate::{
    errors::LoxError,
    parser::{expr::Expression, stmt::Statement},
    types::token::Token,
};

pub mod interpreter;
pub mod printer;

pub trait Visitor<T> {
    fn visit_unary_expr(&mut self, op: &Token, right: &Expression) -> Result<T, LoxError>;
    fn visit_binary_expr(&mut self, left: &Expression, op: &Token, right: &Expression) -> Result<T, LoxError>;
    fn visit_logic_expr(&mut self, left: &Expression, op: &Token, right: &Expression) -> Result<T, LoxError>;
    fn visit_grouping_expr(&mut self, expr: &Expression) -> Result<T, LoxError>;
    fn visit_literal_expr(&mut self, value: &Token) -> Result<T, LoxError>;
    fn visit_variable_expr(&mut self, name: &Token) -> Result<T, LoxError>;
    fn visit_assign_expr(&mut self, name: &Token, right: &Expression) -> Result<T, LoxError>;

    fn visit_expression_stmt(&mut self, expr: &Expression) -> Result<(), LoxError>;
    fn visit_print_stmt(&mut self, expr: &Expression) -> Result<(), LoxError>;
    fn visit_var_stmt(&mut self, name: &Token, init: &Option<Expression>) -> Result<(), LoxError>;
    fn visit_block_stmt(&mut self, statements: &[Statement]) -> Result<(), LoxError>;
    fn visit_if_stmt(
        &mut self,
        cond: &Expression,
        b_then: &Statement,
        b_else: &Option<Box<Statement>>,
    ) -> Result<(), LoxError>;
    fn visit_while_stmt(&mut self, cond: &Expression, stmt: &Statement) -> Result<(), LoxError>;
}
