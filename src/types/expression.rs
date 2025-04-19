use std::{fmt, rc::Rc};

use crate::{errors::LoxError, types::token::Token, visitors::Visitor};

#[derive(Debug, Clone)]
pub enum Expr {
    Binary(Rc<Expr>, Rc<Token>, Rc<Expr>),
    Logical(Rc<Expr>, Rc<Token>, Rc<Expr>),
    Unary(Rc<Token>, Rc<Expr>),
    Grouping(Rc<Expr>),
    Literal(Rc<Token>),
    Variable(Rc<Token>),
    Assign(Rc<Token>, Rc<Expr>),
    Call(Rc<Expr>, Rc<Token>, Vec<Rc<Expr>>),
    Get(Rc<Expr>, Rc<Token>),
    Set(Rc<Expr>, Rc<Token>, Rc<Expr>),
}

impl Expr {
    pub fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> Result<T, LoxError> {
        match self {
            Expr::Literal(token) => visitor.visit_literal_expr(Rc::clone(token)),
            Expr::Binary(left, token, right) => {
                visitor.visit_binary_expr(Rc::clone(left), Rc::clone(token), Rc::clone(right))
            }
            Expr::Grouping(expr) => visitor.visit_grouping_expr(Rc::clone(expr)),
            Expr::Unary(token, right) => visitor.visit_unary_expr(Rc::clone(token), Rc::clone(right)),
            Expr::Variable(token) => visitor.visit_variable_expr(Rc::clone(token)),
            Expr::Assign(token, value) => visitor.visit_assign_expr(Rc::clone(token), Rc::clone(value)),
            Expr::Logical(left, token, right) => {
                visitor.visit_logic_expr(Rc::clone(left), Rc::clone(token), Rc::clone(right))
            }
            Expr::Call(callee, paren, args) => visitor.visit_call_expr(Rc::clone(callee), Rc::clone(paren), args),
            Expr::Get(object, name) => visitor.visit_get_expr(Rc::clone(object), Rc::clone(name)),
            Expr::Set(object, name, value) => {
                visitor.visit_set_expr(Rc::clone(object), Rc::clone(name), Rc::clone(value))
            }
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
            Expr::Get(object, name) => write!(f, "FieldGetter({}.{})", object, name),
            Expr::Set(object, name, value) => write!(f, "FieldSetter({}.{}={})", object, name, value),
        }
    }
}
