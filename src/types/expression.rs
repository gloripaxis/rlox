use std::{fmt, rc::Rc};

use crate::{errors::LoxError, types::token::Token, visitors::Visitor};

#[derive(Debug, Clone)]
pub enum Expr {
    Binary(Box<Expr>, Rc<Token>, Box<Expr>),
    Logical(Box<Expr>, Rc<Token>, Box<Expr>),
    Unary(Rc<Token>, Box<Expr>),
    Grouping(Box<Expr>),
    Literal(Rc<Token>),
    Variable(Rc<Token>),
    Assign(Rc<Token>, Box<Expr>),
    Call(Box<Expr>, Rc<Token>, Vec<Expr>),
}

impl Expr {
    pub fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> Result<T, LoxError> {
        match self {
            Expr::Literal(token) => visitor.visit_literal_expr(token),
            Expr::Binary(left, token, right) => visitor.visit_binary_expr(left, token, right),
            Expr::Grouping(expr) => visitor.visit_grouping_expr(expr),
            Expr::Unary(token, right) => visitor.visit_unary_expr(token, right),
            Expr::Variable(token) => visitor.visit_variable_expr(token),
            Expr::Assign(token, value) => visitor.visit_assign_expr(token, value),
            Expr::Logical(left, token, right) => visitor.visit_logic_expr(left, token, right),
            Expr::Call(callee, paren, args) => visitor.visit_call_expr(callee, paren, args),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Logical(_, tok, _) => write!(f, "LogicalExpression({})", tok.get_lexeme()),
            Expr::Binary(_, tok, _) => write!(f, "BinaryExpression({})", tok.get_lexeme()),
            Expr::Unary(tok, _) => write!(f, "UnaryExpression({})", tok.get_lexeme()),
            Expr::Grouping(ex) => write!(f, "GroupingExpression({})", ex),
            Expr::Literal(tok) => write!(f, "LiteralExpression({})", tok.get_literal()),
            Expr::Variable(tok) => write!(f, "VariableExpression({})", tok.get_lexeme()),
            Expr::Assign(tok, _) => write!(f, "AssignmentExpression({})", tok.get_lexeme()),
            Expr::Call(callee, _, _) => write!(f, "FunctionCall({})", callee),
        }
    }
}
