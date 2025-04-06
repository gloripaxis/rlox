use crate::{
    errors::{ErrorInfo, LoxError},
    types::{
        expression::Expr,
        literal::Lit,
        statement::Stmt,
        token::{Token, TokenType},
    },
};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    program: Vec<Stmt>,
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

    pub fn parse(mut self) -> Result<Vec<Stmt>, LoxError> {
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

    fn declaration(&mut self) -> Result<Stmt, ErrorInfo> {
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

    fn var_decl(&mut self) -> Result<Stmt, ErrorInfo> {
        let name = self.expect(TokenType::Identifier, "Expected variable name")?.clone();
        let mut initializer: Option<Expr> = None;
        if self.advance_maybe(&[TokenType::Assign]) {
            let expr = self.expression()?;
            initializer = Some(expr);
        }
        self.expect(TokenType::Semicolon, "Expected ';' after variable declaration")?;
        Ok(Stmt::Var(name, initializer))
    }

    fn statement(&mut self) -> Result<Stmt, ErrorInfo> {
        if self.advance_maybe(&[TokenType::Print]) {
            return self.print_stmt();
        }
        if self.advance_maybe(&[TokenType::LeftBrace]) {
            return self.block_stmt();
        }
        if self.advance_maybe(&[TokenType::If]) {
            return self.if_stmt();
        }
        if self.advance_maybe(&[TokenType::While]) {
            return self.while_stmt();
        }
        if self.advance_maybe(&[TokenType::For]) {
            return self.for_stmt();
        }
        self.expr_stmt()
    }

    fn print_stmt(&mut self) -> Result<Stmt, ErrorInfo> {
        let expr = self.expression()?;
        self.expect(TokenType::Semicolon, "Expected ';' after a print statement")?;
        Ok(Stmt::Print(expr))
    }

    fn block_stmt(&mut self) -> Result<Stmt, ErrorInfo> {
        let mut statements: Vec<Stmt> = vec![];
        while !self.is_type(TokenType::RightBrace) && !self.is_end() {
            let stmt = self.declaration()?;
            statements.push(stmt);
        }
        self.expect(TokenType::RightBrace, "Expect '}' after block")?;
        Ok(Stmt::Block(statements))
    }

    fn if_stmt(&mut self) -> Result<Stmt, ErrorInfo> {
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

        Ok(Stmt::If(condition, Box::new(then_branch), else_branch))
    }

    fn while_stmt(&mut self) -> Result<Stmt, ErrorInfo> {
        self.expect(TokenType::LeftParen, "Expect '(' after 'while'")?;
        let condition = self.expression()?;
        self.expect(TokenType::RightParen, "Expect ')' after 'while' condition")?;
        let stmts = self.statement()?;
        Ok(Stmt::While(condition, Box::new(stmts)))
    }

    fn for_stmt(&mut self) -> Result<Stmt, ErrorInfo> {
        self.expect(TokenType::LeftParen, "Expected '(' after 'for'")?;

        // Parse initializer
        let orig_init: Option<Stmt>;
        if self.advance_maybe(&[TokenType::Semicolon]) {
            orig_init = None;
        } else if self.advance_maybe(&[TokenType::Var]) {
            orig_init = Some(self.var_decl()?);
        } else {
            orig_init = Some(self.expr_stmt()?);
        }

        // Parse condition
        let orig_cond = match self.is_type(TokenType::Semicolon) {
            true => None,
            false => Some(self.expression()?),
        };
        self.expect(TokenType::Semicolon, "Expected ';' after loop condition")?;

        // Parse increment
        let orig_inc = match self.is_type(TokenType::RightParen) {
            true => None,
            false => Some(self.expression()?),
        };
        self.expect(TokenType::RightParen, "Expect ')' after for clauses")?;

        // Parse statement
        let orig_body = self.statement()?;

        // if it exists, add the increment to the end of the while-body
        let body = match orig_inc {
            None => orig_body,
            Some(inc) => Stmt::Block(vec![orig_body, Stmt::Expression(inc)]),
        };

        // if not explicitly provided, set the condition to perma-true
        let cond = match orig_cond {
            Some(expr) => expr,
            None => {
                let fake_token = Token::new(TokenType::True, Lit::Bool(true), self.peek().get_position());
                Expr::Literal(fake_token)
            }
        };

        // if it exists, add the initializer before the while-body
        let result = match orig_init {
            None => Stmt::While(cond, Box::new(body)),
            Some(init) => Stmt::Block(vec![init, Stmt::While(cond, Box::new(body))]),
        };
        Ok(result)
    }

    fn expr_stmt(&mut self) -> Result<Stmt, ErrorInfo> {
        let expr = self.expression()?;
        self.expect(TokenType::Semicolon, "Expected ';' after an expression statement")?;
        Ok(Stmt::Expression(expr))
    }

    fn expression(&mut self) -> Result<Expr, ErrorInfo> {
        self.assign_expr()
    }

    fn assign_expr(&mut self) -> Result<Expr, ErrorInfo> {
        let expr = self.or_expr()?;

        if self.advance_maybe(&[TokenType::Assign]) {
            let eq_token = self.previous();
            return match expr {
                Expr::Variable(name) => {
                    let value = self.assign_expr()?;
                    Ok(Expr::Assign(name, Box::new(value)))
                }
                _ => Err(ErrorInfo::from_token(
                    eq_token,
                    format!("Invalid assignment target: {}", expr),
                )),
            };
        }

        Ok(expr)
    }

    fn or_expr(&mut self) -> Result<Expr, ErrorInfo> {
        let mut expr = self.and_expr()?;

        while self.advance_maybe(&[TokenType::Or]) {
            let operator = self.previous().clone();
            let right = self.and_expr()?;
            expr = Expr::Logical(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn and_expr(&mut self) -> Result<Expr, ErrorInfo> {
        let mut expr = self.equality_expr()?;

        while self.advance_maybe(&[TokenType::And]) {
            let operator = self.previous().clone();
            let right = self.equality_expr()?;
            expr = Expr::Logical(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn equality_expr(&mut self) -> Result<Expr, ErrorInfo> {
        let mut expr: Expr = self.comparison_expr()?;
        while self.advance_maybe(&[TokenType::Neq, TokenType::Eq]) {
            let token = self.previous().clone();
            let right = self.comparison_expr()?;
            expr = Expr::Binary(Box::new(expr), token, Box::new(right));
        }
        Ok(expr)
    }

    fn comparison_expr(&mut self) -> Result<Expr, ErrorInfo> {
        let mut expr = self.term_expr()?;

        while self.advance_maybe(&[TokenType::Gt, TokenType::Geq, TokenType::Lt, TokenType::Leq]) {
            let token = self.previous().clone();
            let right = self.term_expr()?;
            expr = Expr::Binary(Box::new(expr), token, Box::new(right));
        }
        Ok(expr)
    }

    fn term_expr(&mut self) -> Result<Expr, ErrorInfo> {
        let mut expr = self.factor_expr()?;

        while self.advance_maybe(&[TokenType::Minus, TokenType::Plus]) {
            let token = self.previous().clone();
            let right = self.factor_expr()?;
            expr = Expr::Binary(Box::new(expr), token, Box::new(right));
        }
        Ok(expr)
    }

    fn factor_expr(&mut self) -> Result<Expr, ErrorInfo> {
        let mut expr = self.unary_expr()?;

        while self.advance_maybe(&[TokenType::Slash, TokenType::Star]) {
            let token = self.previous().clone();
            let right = self.unary_expr()?;
            expr = Expr::Binary(Box::new(expr), token, Box::new(right));
        }
        Ok(expr)
    }

    fn unary_expr(&mut self) -> Result<Expr, ErrorInfo> {
        if self.advance_maybe(&[TokenType::Bang, TokenType::Minus]) {
            let token = self.previous().clone();
            let right = self.unary_expr()?;
            return Ok(Expr::Unary(token, Box::new(right)));
        }
        self.primary_expr()
    }

    fn primary_expr(&mut self) -> Result<Expr, ErrorInfo> {
        let token = self.peek().clone();
        match token.get_type() {
            TokenType::False => {
                self.advance();
                Ok(Expr::Literal(token))
            }
            TokenType::True => {
                self.advance();
                Ok(Expr::Literal(token))
            }
            TokenType::Nil => {
                self.advance();
                Ok(Expr::Literal(token))
            }
            TokenType::Number | TokenType::String => {
                self.advance();
                Ok(Expr::Literal(token))
            }
            TokenType::Identifier => {
                self.advance();
                Ok(Expr::Variable(token))
            }
            TokenType::LeftParen => {
                self.advance();
                let expr = self.expression()?;
                self.expect(TokenType::RightParen, "Missing closing parenthesis")?;
                Ok(Expr::Grouping(Box::new(expr)))
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
