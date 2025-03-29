use crate::lexer::token::{Literal, TokenType};

#[derive(Debug, Clone)]
pub enum Expression {
    Binary(Box<Expression>, TokenType, Box<Expression>),
    Unary(TokenType, Box<Expression>),
    Grouping(Box<Expression>),
    Literal(Literal),
}
