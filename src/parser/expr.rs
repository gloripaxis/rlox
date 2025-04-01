use std::fmt;

use crate::{errors::LoxError, lexer::token::Token, visitors::Visitor};

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
    pub fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> Result<T, LoxError> {
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

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Logical(_, tok, _) => write!(f, "LogicalExpression({})", tok.get_lexeme()),
            Expression::Binary(_, tok, _) => write!(f, "BinaryExpression({})", tok.get_lexeme()),
            Expression::Unary(tok, _) => write!(f, "UnaryExpression({})", tok.get_lexeme()),
            Expression::Grouping(ex) => write!(f, "GroupingExpression({})", ex),
            Expression::Literal(tok) => write!(f, "LiteralExpression({})", tok.get_literal()),
            Expression::Variable(tok) => write!(f, "VariableExpression({})", tok.get_lexeme()),
            Expression::Assign(tok, _) => write!(f, "AssignmentExpression({})", tok.get_lexeme()),
        }
    }
}
