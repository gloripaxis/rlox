use crate::lexer::token::Token;

#[derive(Debug, Clone)]
pub enum Expression {
    Binary(Box<Expression>, Token, Box<Expression>),
    Unary(Token, Box<Expression>),
    Grouping(Box<Expression>),
    Literal(Token),
    Variable(Token),
    Assign(Token, Box<Expression>),
}
