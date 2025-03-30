use std::error::Error;

use expr::Expression;
use stmt::Statement;

use crate::{
    errors::{ErrorMessage, ErrorType, RloxError},
    lexer::token::{Token, TokenType},
};

pub mod expr;
pub mod stmt;

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    program: Vec<Statement>,
    errors: Vec<ErrorMessage>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0,
            program: vec![],
            errors: vec![],
        }
    }

    pub fn parse(mut self) -> Result<Vec<Statement>, Box<dyn Error>> {
        while !self.is_end() {
            let stmt = self.declaration();
            if let Some(x) = stmt {
                self.program.push(x);
            }
        }

        if !self.errors.is_empty() {
            Err(self.build_error())
        } else {
            Ok(self.program)
        }
    }

    fn declaration(&mut self) -> Option<Statement> {
        let result = match self.advance_maybe(&[TokenType::Var]) {
            true => self.var_decl(),
            false => self.statement(),
        };
        match result {
            Ok(x) => Some(x),
            Err(x) => {
                self.errors.push(x);
                self.synchronize();
                None
            }
        }
    }

    fn var_decl(&mut self) -> Result<Statement, ErrorMessage> {
        let name = self.expect(TokenType::Identifier, "Expected variable name")?.clone();
        let mut initializer: Option<Expression> = None;
        if self.advance_maybe(&[TokenType::Assign]) {
            let expr = self.expression()?;
            initializer = Some(expr);
        }
        self.expect(TokenType::Semicolon, "Expected ';' after variable declaration")?;
        Ok(Statement::Var(name, initializer))
    }

    fn statement(&mut self) -> Result<Statement, ErrorMessage> {
        if self.advance_maybe(&[TokenType::Print]) {
            return self.print_stmt();
        }
        if self.advance_maybe(&[TokenType::LeftBrace]) {
            return self.block_stmt();
        }
        self.expr_stmt()
    }

    fn print_stmt(&mut self) -> Result<Statement, ErrorMessage> {
        let expr = self.expression()?;
        self.expect(TokenType::Semicolon, "Expected ';' after a print statement")?;
        Ok(Statement::Print(expr))
    }

    fn block_stmt(&mut self) -> Result<Statement, ErrorMessage> {
        let mut statements: Vec<Statement> = vec![];
        while !self.is_type(TokenType::RightBrace) && !self.is_end() {
            let stmt = self.declaration();
            match stmt {
                None => {}
                Some(s) => statements.push(s),
            }
        }
        self.expect(TokenType::RightBrace, "Expect '}' after block")?;
        Ok(Statement::Block(statements))
    }

    fn expr_stmt(&mut self) -> Result<Statement, ErrorMessage> {
        let expr = self.expression()?;
        self.expect(TokenType::Semicolon, "Expected ';' after an expression statement")?;
        Ok(Statement::Expression(expr))
    }

    fn expression(&mut self) -> Result<Expression, ErrorMessage> {
        self.assign_expr()
    }

    fn assign_expr(&mut self) -> Result<Expression, ErrorMessage> {
        let expr = self.equality_expr()?;

        if self.advance_maybe(&[TokenType::Assign]) {
            let eq_token = self.previous();
            let (line, column) = eq_token.get_location();
            let value = self.assign_expr()?;

            if let Expression::Variable(name) = expr {
                return Ok(Expression::Assign(name, Box::new(value)));
            } else {
                let emsg = ErrorMessage::new(
                    ErrorType::Syntax,
                    String::from("Invalid assignment target"),
                    line,
                    column,
                );
                return Err(emsg);
            }
        }

        Ok(expr)
    }

    fn equality_expr(&mut self) -> Result<Expression, ErrorMessage> {
        let mut expr: Expression = self.comparison_expr()?;
        while self.advance_maybe(&[TokenType::Neq, TokenType::Eq]) {
            let token = self.previous().clone();
            let right = self.comparison_expr()?;
            expr = Expression::Binary(Box::new(expr), token, Box::new(right));
        }
        Ok(expr)
    }

    fn comparison_expr(&mut self) -> Result<Expression, ErrorMessage> {
        let mut expr = self.term_expr()?;

        while self.advance_maybe(&[TokenType::Gt, TokenType::Geq, TokenType::Lt, TokenType::Leq]) {
            let token = self.previous().clone();
            let right = self.term_expr()?;
            expr = Expression::Binary(Box::new(expr), token, Box::new(right));
        }
        Ok(expr)
    }

    fn term_expr(&mut self) -> Result<Expression, ErrorMessage> {
        let mut expr = self.factor_expr()?;

        while self.advance_maybe(&[TokenType::Minus, TokenType::Plus]) {
            let token = self.previous().clone();
            let right = self.factor_expr()?;
            expr = Expression::Binary(Box::new(expr), token, Box::new(right));
        }
        Ok(expr)
    }

    fn factor_expr(&mut self) -> Result<Expression, ErrorMessage> {
        let mut expr = self.unary_expr()?;

        while self.advance_maybe(&[TokenType::Slash, TokenType::Star]) {
            let token = self.previous().clone();
            let right = self.unary_expr()?;
            expr = Expression::Binary(Box::new(expr), token, Box::new(right));
        }
        Ok(expr)
    }

    fn unary_expr(&mut self) -> Result<Expression, ErrorMessage> {
        if self.advance_maybe(&[TokenType::Bang, TokenType::Minus]) {
            let token = self.previous().clone();
            let right = self.unary_expr()?;
            return Ok(Expression::Unary(token, Box::new(right)));
        }
        self.primary_expr()
    }

    fn primary_expr(&mut self) -> Result<Expression, ErrorMessage> {
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
            TokenType::Identifier => {
                self.advance();
                Expression::Variable(token)
            }
            TokenType::LeftParen => {
                self.advance();
                let expr = self.expression()?;
                self.expect(TokenType::RightParen, "Missing closing parenthesis")?;
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
                return Err(emsg);
            }
        };
        Ok(expr)
    }

    fn synchronize(&mut self) {
        self.advance();

        while !self.is_end() {
            if self.previous().get_type() == TokenType::Semicolon {
                return;
            }

            match self.peek().get_type() {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => return,
                _ => self.advance(),
            };
        }
    }

    fn expect(&mut self, tt: TokenType, msg: &str) -> Result<&Token, ErrorMessage> {
        if self.is_type(tt) {
            return Ok(self.advance());
        }
        let token = if self.is_end() { self.previous() } else { self.peek() };
        let (line, column) = token.get_location();
        let lexeme = token.get_lexeme();

        let emsg = ErrorMessage::new(ErrorType::Syntax, format!("{msg} at '{lexeme}'"), line, column);
        Err(emsg)
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

    fn build_error(&mut self) -> Box<dyn Error> {
        Box::new(RloxError::new(self.errors.clone()))
    }
}
