use std::error::Error;

use expr::Expression;

use crate::{
    errors::{ErrorMessage, ErrorType, RloxError},
    lexer::token::{Token, TokenType},
};

pub mod expr;

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> Result<Expression, Box<dyn Error>> {
        self.expression()
    }

    fn expression(&mut self) -> Result<Expression, Box<dyn Error>> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expression, Box<dyn Error>> {
        let mut expr: Expression = self.comparison()?;
        while self.advance_maybe(&[TokenType::Neq, TokenType::Eq]) {
            let token = self.previous().clone();
            let right = self.comparison()?;
            expr = Expression::Binary(Box::new(expr), token, Box::new(right));
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expression, Box<dyn Error>> {
        let mut expr = self.term()?;

        while self.advance_maybe(&[TokenType::Gt, TokenType::Geq, TokenType::Lt, TokenType::Leq]) {
            let token = self.previous().clone();
            let right = self.term()?;
            expr = Expression::Binary(Box::new(expr), token, Box::new(right));
        }
        Ok(expr)
    }

    fn term(&mut self) -> Result<Expression, Box<dyn Error>> {
        let mut expr = self.factor()?;

        while self.advance_maybe(&[TokenType::Minus, TokenType::Plus]) {
            let token = self.previous().clone();
            let right = self.factor()?;
            expr = Expression::Binary(Box::new(expr), token, Box::new(right));
        }
        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expression, Box<dyn Error>> {
        let mut expr = self.unary()?;

        while self.advance_maybe(&[TokenType::Slash, TokenType::Star]) {
            let token = self.previous().clone();
            let right = self.unary()?;
            expr = Expression::Binary(Box::new(expr), token, Box::new(right));
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expression, Box<dyn Error>> {
        if self.advance_maybe(&[TokenType::Bang, TokenType::Minus]) {
            let token = self.previous().clone();
            let right = self.unary()?;
            return Ok(Expression::Unary(token, Box::new(right)));
        }
        self.primary()
    }

    fn primary(&mut self) -> Result<Expression, Box<dyn Error>> {
        let token = self.peek().clone();
        let expr = match token.get_type() {
            TokenType::False => {
                self.advance();
                Expression::Literal(token)
            }
            TokenType::True => {
                self.advance();
                Expression::Literal(token)
            }
            TokenType::Nil => {
                self.advance();
                Expression::Literal(token)
            }
            TokenType::Number | TokenType::String => {
                self.advance();
                Expression::Literal(token)
            }
            TokenType::LeftParen => {
                self.advance();
                let expr = self.expression()?;
                self.expect(TokenType::RightParen)?;
                Expression::Grouping(Box::new(expr))
            }
            _ => {
                let token = self.peek();
                let (line, column) = token.get_location();
                let lexeme = token.get_lexeme();
                let emsg = ErrorMessage::new(
                    ErrorType::Syntax,
                    format!("Expected expression at '{lexeme}'"),
                    line,
                    column,
                );
                return Err(Box::new(RloxError::new(vec![emsg])));
            }
        };
        Ok(expr)
    }

    fn expect(&mut self, tt: TokenType) -> Result<&Token, Box<dyn Error>> {
        if self.is_type(tt) {
            return Ok(self.advance());
        }
        let token = self.peek();
        let (line, column) = token.get_location();
        let lexeme = token.get_lexeme();

        let emsg = ErrorMessage::new(
            ErrorType::Syntax,
            format!("Missing closing parenthesis at '{lexeme}'"),
            line,
            column,
        );
        Err(Box::new(RloxError::new(vec![emsg])))
    }

    fn advance(&mut self) -> &Token {
        if !self.is_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn advance_maybe(&mut self, types: &[TokenType]) -> bool {
        for tt in types {
            if self.is_type(*tt) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn is_type(&self, ttype: TokenType) -> bool {
        if self.is_end() {
            return false;
        }
        ttype == self.peek().get_type()
    }

    fn peek(&self) -> &Token {
        self.tokens.get(self.current).unwrap()
    }

    fn previous(&self) -> &Token {
        self.tokens.get(self.current - 1).unwrap()
    }

    fn is_end(&self) -> bool {
        if let TokenType::EndOfFile = self.peek().get_type() {
            return true;
        }
        false
    }
}
