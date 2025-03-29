use std::error::Error;

use crate::{
    errors::{ErrorMessage, ErrorType, RloxError},
    lexer::token::{Literal, Token, TokenType},
    parser::expr::Expression,
};

fn unary_number_error(op_token: Token, right: Literal) -> RloxError {
    let (line, column) = op_token.get_location();
    let emsg = ErrorMessage::new(
        ErrorType::RuntimeError,
        format!(
            "Operand of '{}' must be a number. Found {:?}",
            op_token.get_lexeme(),
            right
        ),
        line,
        column,
    );
    RloxError::new(vec![emsg])
}

fn binary_number_error(op_token: Token, left: Literal, right: Literal) -> RloxError {
    let (line, column) = op_token.get_location();
    let emsg = ErrorMessage::new(
        ErrorType::RuntimeError,
        format!(
            "Operands of '{}' must be numbers. Found {:?} and {:?}",
            op_token.get_lexeme(),
            left,
            right
        ),
        line,
        column,
    );
    RloxError::new(vec![emsg])
}

fn plus_error(op_token: Token, left: Literal, right: Literal) -> RloxError {
    let (line, column) = op_token.get_location();
    let emsg = ErrorMessage::new(
        ErrorType::RuntimeError,
        format!(
            "Operands of + must be either both strings or both numbers. Found {:?} and {:?}",
            left, right
        ),
        line,
        column,
    );
    RloxError::new(vec![emsg])
}

fn invalid_binary_operator(op_token: Token) -> RloxError {
    let (line, column) = op_token.get_location();
    let emsg = ErrorMessage::new(
        ErrorType::RuntimeError,
        format!("(CRITICAL) Invalid binary operator detected: {}", op_token.get_lexeme()),
        line,
        column,
    );
    RloxError::new(vec![emsg])
}

fn invalid_unary_operator(op_token: Token) -> RloxError {
    let (line, column) = op_token.get_location();
    let emsg = ErrorMessage::new(
        ErrorType::RuntimeError,
        format!("(CRITICAL) Invalid unnary operator detected: {}", op_token.get_lexeme()),
        line,
        column,
    );
    RloxError::new(vec![emsg])
}

pub fn visit(expr: Expression) -> Result<Literal, Box<dyn Error>> {
    match expr {
        Expression::Literal(token) => Ok(token.get_literal()),
        Expression::Binary(left_expr, token, right_expr) => {
            let left = visit(*left_expr)?;
            let right = visit(*right_expr)?;

            match token.get_type() {
                TokenType::Minus => match (&left, &right) {
                    (Literal::Number(l), Literal::Number(r)) => Ok(Literal::Number(l - r)),
                    _ => Err(Box::new(binary_number_error(token, left, right))),
                },
                TokenType::Plus => match (&left, &right) {
                    (Literal::Number(l), Literal::Number(r)) => Ok(Literal::Number(l + r)),
                    (Literal::String(l), Literal::String(r)) => Ok(Literal::String(format!("{l}{r}"))),
                    _ => Err(Box::new(plus_error(token, left, right))),
                },
                TokenType::Slash => match (&left, &right) {
                    (Literal::Number(l), Literal::Number(r)) => Ok(Literal::Number(l / r)),
                    _ => Err(Box::new(binary_number_error(token, left, right))),
                },
                TokenType::Star => match (&left, &right) {
                    (Literal::Number(l), Literal::Number(r)) => Ok(Literal::Number(l * r)),
                    _ => Err(Box::new(binary_number_error(token, left, right))),
                },
                TokenType::Gt => match (&left, &right) {
                    (Literal::Number(l), Literal::Number(r)) => Ok(Literal::Boolean(l > r)),
                    _ => Err(Box::new(binary_number_error(token, left, right))),
                },
                TokenType::Geq => match (&left, &right) {
                    (Literal::Number(l), Literal::Number(r)) => Ok(Literal::Boolean(l >= r)),
                    _ => Err(Box::new(binary_number_error(token, left, right))),
                },
                TokenType::Lt => match (&left, &right) {
                    (Literal::Number(l), Literal::Number(r)) => Ok(Literal::Boolean(l < r)),
                    _ => Err(Box::new(binary_number_error(token, left, right))),
                },
                TokenType::Leq => match (&left, &right) {
                    (Literal::Number(l), Literal::Number(r)) => Ok(Literal::Boolean(l <= r)),
                    _ => Err(Box::new(binary_number_error(token, left, right))),
                },
                TokenType::Eq => Ok(Literal::Boolean(is_equal(left, right))),
                TokenType::Neq => Ok(Literal::Boolean(!is_equal(left, right))),
                _ => Err(Box::new(invalid_binary_operator(token))),
            }
        }
        Expression::Grouping(expr) => visit(*expr),
        Expression::Unary(token, right_expr) => {
            let right = visit(*right_expr)?;
            match token.get_type() {
                TokenType::Minus => match right {
                    Literal::Number(x) => Ok(Literal::Number(-x)),
                    _ => Err(Box::new(unary_number_error(token, right))),
                },
                TokenType::Bang => Ok(Literal::Boolean(!is_truthy(right))),
                _ => Err(Box::new(invalid_unary_operator(token))),
            }
        }
    }
}

fn is_truthy(literal: Literal) -> bool {
    match literal {
        Literal::Nil => false,
        Literal::Boolean(x) => x,
        _ => true,
    }
}

fn is_equal(left: Literal, right: Literal) -> bool {
    match (left, right) {
        (Literal::Nil, Literal::Nil) => true,
        (Literal::Nil, _) => false,
        (_, Literal::Nil) => false,
        (Literal::String(x), Literal::String(y)) => x == y,
        (Literal::String(_), _) => false,
        (_, Literal::String(_)) => false,
        (Literal::Number(x), Literal::Number(y)) => x == y,
        (Literal::Number(_), _) => false,
        (_, Literal::Number(_)) => false,
        (Literal::Boolean(x), Literal::Boolean(y)) => x == y,
    }
}
