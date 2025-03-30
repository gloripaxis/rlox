use super::expr::Expression;
use crate::lexer::token::Token;

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(Expression),
    Print(Expression),
    Var(Token, Option<Expression>),
}
