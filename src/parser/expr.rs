use std::error::Error;

use crate::{lexer::token::Token, visitors::Visitor};

#[derive(Debug, Clone)]
pub enum Expression {
    Binary(Box<Expression>, Token, Box<Expression>),
    Logical(Box<Expression>, Token, Box<Expression>),
    Unary(Token, Box<Expression>),
    Grouping(Box<Expression>),
    Literal(Token),
    Variable(Token),
    Assign(Token, Box<Expression>),
}

impl Expression {
    pub fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> Result<T, Box<dyn Error>> {
        match self {
            Expression::Literal(token) => visitor.visit_literal_expr(token),
            Expression::Binary(left, token, right) => visitor.visit_binary_expr(left, token, right),
            Expression::Grouping(expr) => visitor.visit_grouping_expr(expr),
            Expression::Unary(token, right) => visitor.visit_unary_expr(token, right),
            Expression::Variable(token) => visitor.visit_variable_expr(token),
            Expression::Assign(token, value) => visitor.visit_assign_expr(token, value),
            Expression::Logical(left, token, right) => visitor.visit_logic_expr(left, token, right),
        }
    }
}
