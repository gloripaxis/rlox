use std::error::Error;

use crate::{
    lexer::token::Token,
    parser::{expr::Expression, stmt::Statement},
};

pub mod interpreter;
pub mod printer;

pub trait Visitor<T> {
    fn visit_unary_expr(&mut self, op: &Token, right: &Expression) -> Result<T, Box<dyn Error>>;
    fn visit_binary_expr(&mut self, left: &Expression, op: &Token, right: &Expression) -> Result<T, Box<dyn Error>>;
    fn visit_logic_expr(&mut self, left: &Expression, op: &Token, right: &Expression) -> Result<T, Box<dyn Error>>;
    fn visit_grouping_expr(&mut self, expr: &Expression) -> Result<T, Box<dyn Error>>;
    fn visit_literal_expr(&mut self, value: &Token) -> Result<T, Box<dyn Error>>;
    fn visit_variable_expr(&mut self, name: &Token) -> Result<T, Box<dyn Error>>;
    fn visit_assign_expr(&mut self, name: &Token, right: &Expression) -> Result<T, Box<dyn Error>>;

    fn visit_expression_stmt(&mut self, expr: &Expression) -> Result<(), Box<dyn Error>>;
    fn visit_print_stmt(&mut self, expr: &Expression) -> Result<(), Box<dyn Error>>;
    fn visit_var_stmt(&mut self, name: &Token, init: &Option<Expression>) -> Result<(), Box<dyn Error>>;
    fn visit_block_stmt(&mut self, statements: &[Statement]) -> Result<(), Box<dyn Error>>;
    fn visit_if_stmt(
        &mut self,
        cond: &Expression,
        b_then: &Statement,
        b_else: &Option<Box<Statement>>,
    ) -> Result<(), Box<dyn Error>>;
}
