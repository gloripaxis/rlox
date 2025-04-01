use expr::Expression;
use stmt::Statement;

use crate::{
    errors::{ErrorInfo, LoxError},
    lexer::token::{Token, TokenType},
};

pub mod expr;
pub mod stmt;

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    program: Vec<Statement>,
    errors: Vec<ErrorInfo>,
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

    pub fn parse(mut self) -> Result<Vec<Statement>, LoxError> {
        while !self.is_end() {
            let result = self.declaration();
            match result {
                Ok(statement) => self.program.push(statement),
                Err(einfo) => self.errors.push(einfo),
            }
        }

        if !self.errors.is_empty() {
            Err(LoxError::Syntax(self.errors))
        } else {
            Ok(self.program)
        }
    }

    fn declaration(&mut self) -> Result<Statement, ErrorInfo> {
        let result = match self.advance_maybe(&[TokenType::Var]) {
            true => self.var_decl(),
            false => self.statement(),
        };
        match result {
            Ok(x) => Ok(x),
            Err(x) => {
                self.synchronize();
                Err(x)
            }
        }
    }

    fn var_decl(&mut self) -> Result<Statement, ErrorInfo> {
        let name = self.expect(TokenType::Identifier, "Expected variable name")?.clone();
        let mut initializer: Option<Expression> = None;
        if self.advance_maybe(&[TokenType::Assign]) {
            let expr = self.expression()?;
            initializer = Some(expr);
        }
        self.expect(TokenType::Semicolon, "Expected ';' after variable declaration")?;
        Ok(Statement::Var(name, initializer))
    }

    fn statement(&mut self) -> Result<Statement, ErrorInfo> {
        if self.advance_maybe(&[TokenType::Print]) {
            return self.print_stmt();
        }
        if self.advance_maybe(&[TokenType::LeftBrace]) {
            return self.block_stmt();
        }
        if self.advance_maybe(&[TokenType::If]) {
            return self.if_stmt();
        }
        self.expr_stmt()
    }

    fn print_stmt(&mut self) -> Result<Statement, ErrorInfo> {
        let expr = self.expression()?;
        self.expect(TokenType::Semicolon, "Expected ';' after a print statement")?;
        Ok(Statement::Print(expr))
    }

    fn block_stmt(&mut self) -> Result<Statement, ErrorInfo> {
        let mut statements: Vec<Statement> = vec![];
        while !self.is_type(TokenType::RightBrace) && !self.is_end() {
            let stmt = self.declaration()?;
            statements.push(stmt);
        }
        self.expect(TokenType::RightBrace, "Expect '}' after block")?;
        Ok(Statement::Block(statements))
    }

    fn if_stmt(&mut self) -> Result<Statement, ErrorInfo> {
        self.expect(TokenType::LeftParen, "Expect '(' after 'if'")?;
        let condition = self.expression()?;
        self.expect(TokenType::RightParen, "Expect ')' after 'if' condition")?;

        let then_branch = self.statement()?;
        let else_branch = match self.advance_maybe(&[TokenType::Else]) {
            true => {
                let else_stmt = self.statement()?;
                Some(Box::new(else_stmt))
            }
            false => None,
        };

        Ok(Statement::If(condition, Box::new(then_branch), else_branch))
    }

    fn expr_stmt(&mut self) -> Result<Statement, ErrorInfo> {
        let expr = self.expression()?;
        self.expect(TokenType::Semicolon, "Expected ';' after an expression statement")?;
        Ok(Statement::Expression(expr))
    }

    fn expression(&mut self) -> Result<Expression, ErrorInfo> {
        self.assign_expr()
    }

    fn assign_expr(&mut self) -> Result<Expression, ErrorInfo> {
        let expr = self.or_expr()?;

        if self.advance_maybe(&[TokenType::Assign]) {
            let eq_token = self.previous();
            return match expr {
                Expression::Variable(name) => {
                    let value = self.assign_expr()?;
                    Ok(Expression::Assign(name, Box::new(value)))
                }
                _ => Err(ErrorInfo::from_token(
                    eq_token,
                    format!("Invalid assignment target: {}", expr),
                )),
            };
        }

        Ok(expr)
    }

    fn or_expr(&mut self) -> Result<Expression, ErrorInfo> {
        let mut expr = self.and_expr()?;

        while self.advance_maybe(&[TokenType::Or]) {
            let operator = self.previous().clone();
            let right = self.and_expr()?;
            expr = Expression::Logical(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn and_expr(&mut self) -> Result<Expression, ErrorInfo> {
        let mut expr = self.equality_expr()?;

        while self.advance_maybe(&[TokenType::And]) {
            let operator = self.previous().clone();
            let right = self.equality_expr()?;
            expr = Expression::Logical(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn equality_expr(&mut self) -> Result<Expression, ErrorInfo> {
        let mut expr: Expression = self.comparison_expr()?;
        while self.advance_maybe(&[TokenType::Neq, TokenType::Eq]) {
            let token = self.previous().clone();
            let right = self.comparison_expr()?;
            expr = Expression::Binary(Box::new(expr), token, Box::new(right));
        }
        Ok(expr)
    }

    fn comparison_expr(&mut self) -> Result<Expression, ErrorInfo> {
        let mut expr = self.term_expr()?;

        while self.advance_maybe(&[TokenType::Gt, TokenType::Geq, TokenType::Lt, TokenType::Leq]) {
            let token = self.previous().clone();
            let right = self.term_expr()?;
            expr = Expression::Binary(Box::new(expr), token, Box::new(right));
        }
        Ok(expr)
    }

    fn term_expr(&mut self) -> Result<Expression, ErrorInfo> {
        let mut expr = self.factor_expr()?;

        while self.advance_maybe(&[TokenType::Minus, TokenType::Plus]) {
            let token = self.previous().clone();
            let right = self.factor_expr()?;
            expr = Expression::Binary(Box::new(expr), token, Box::new(right));
        }
        Ok(expr)
    }

    fn factor_expr(&mut self) -> Result<Expression, ErrorInfo> {
        let mut expr = self.unary_expr()?;

        while self.advance_maybe(&[TokenType::Slash, TokenType::Star]) {
            let token = self.previous().clone();
            let right = self.unary_expr()?;
            expr = Expression::Binary(Box::new(expr), token, Box::new(right));
        }
        Ok(expr)
    }

    fn unary_expr(&mut self) -> Result<Expression, ErrorInfo> {
        if self.advance_maybe(&[TokenType::Bang, TokenType::Minus]) {
            let token = self.previous().clone();
            let right = self.unary_expr()?;
            return Ok(Expression::Unary(token, Box::new(right)));
        }
        self.primary_expr()
    }

    fn primary_expr(&mut self) -> Result<Expression, ErrorInfo> {
        let token = self.peek().clone();
        match token.get_type() {
            TokenType::False => {
                self.advance();
                Ok(Expression::Literal(token))
            }
            TokenType::True => {
                self.advance();
                Ok(Expression::Literal(token))
            }
            TokenType::Nil => {
                self.advance();
                Ok(Expression::Literal(token))
            }
            TokenType::Number | TokenType::String => {
                self.advance();
                Ok(Expression::Literal(token))
            }
            TokenType::Identifier => {
                self.advance();
                Ok(Expression::Variable(token))
            }
            TokenType::LeftParen => {
                self.advance();
                let expr = self.expression()?;
                self.expect(TokenType::RightParen, "Missing closing parenthesis")?;
                Ok(Expression::Grouping(Box::new(expr)))
            }
            _ => {
                let message = format!("Expected expression at '{}'", self.peek().get_lexeme());
                Err(ErrorInfo::from_token(&token, message))
            }
        }
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

    fn expect(&mut self, tt: TokenType, msg: &str) -> Result<&Token, ErrorInfo> {
        if self.is_type(tt) {
            return Ok(self.advance());
        }

        let token = if self.is_end() { self.previous() } else { self.peek() };
        let lexeme = token.get_lexeme();

        Err(ErrorInfo::from_token(token, format!("{msg} at '{lexeme}'")))
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
