use std::rc::Rc;

use crate::{
    errors::LoxError,
    types::{
        expression::Expr,
        literal::Lit,
        statement::Stmt,
        token::{Token, TokenType},
    },
};

pub struct Parser {
    tokens: Vec<Rc<Token>>,
    current: usize,
    program: Vec<Rc<Stmt>>,
    errors: Vec<LoxError>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        let rc_tokens: Vec<Rc<Token>> = tokens.into_iter().map(Rc::new).collect();
        Self {
            tokens: rc_tokens,
            current: 0,
            program: vec![],
            errors: vec![],
        }
    }

    pub fn parse(mut self) -> Result<Vec<Rc<Stmt>>, Vec<LoxError>> {
        while !self.is_end() {
            let result = self.declaration();
            match result {
                Ok(statement) => self.program.push(Rc::new(statement)),
                Err(einfo) => self.errors.push(einfo),
            }
        }

        if !self.errors.is_empty() {
            Err(self.errors)
        } else {
            Ok(self.program)
        }
    }

    fn declaration(&mut self) -> Result<Stmt, LoxError> {
        let result = if self.advance_maybe(&[TokenType::Fun]) {
            self.fun_decl("function")
        } else if self.advance_maybe(&[TokenType::Var]) {
            self.var_decl()
        } else {
            self.statement()
        };

        match result {
            Ok(x) => Ok(x),
            Err(x) => {
                self.synchronize();
                Err(x)
            }
        }
    }

    fn fun_decl(&mut self, kind: &'static str) -> Result<Stmt, LoxError> {
        // Read function name
        let name = self.expect(TokenType::Identifier, &format!("{kind} name"), &format!("'{kind}'"))?;

        // Read opening parenthesis
        self.expect(TokenType::LeftParen, "(", &format!("{kind} name"))?;

        // Read parameters
        let mut params: Vec<Rc<Token>> = vec![];
        if !self.is_type(TokenType::RightParen) {
            loop {
                if params.len() >= 255 {
                    return Err(LoxError::too_many_params(self.peek().get_position()));
                }
                let id = self.expect(
                    TokenType::Identifier,
                    "parameter name",
                    "opening ( of function parameter list",
                )?;
                params.push(id);
                if !self.advance_maybe(&[TokenType::Comma]) {
                    break;
                }
            }
        }

        // Read closing parenthesis
        self.expect(TokenType::RightParen, ")", "function parameter list")?;

        // Read function body
        self.expect(TokenType::LeftBrace, "{", "function signature")?;
        let body = self.block_stmt()?;

        // Return function statement
        Ok(Stmt::Function(Rc::clone(&name), params, vec![Rc::new(body)]))
    }

    fn var_decl(&mut self) -> Result<Stmt, LoxError> {
        let name = self.expect(TokenType::Identifier, "variable name", "'var'")?;
        let mut initializer: Option<Rc<Expr>> = None;
        if self.advance_maybe(&[TokenType::Assign]) {
            let expr = self.expression()?;
            initializer = Some(Rc::new(expr));
        }
        self.expect(TokenType::Semicolon, ";", "variable declaration")?;
        Ok(Stmt::Var(name, initializer))
    }

    fn statement(&mut self) -> Result<Stmt, LoxError> {
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
        if self.advance_maybe(&[TokenType::Return]) {
            return self.return_stmt();
        }
        self.expr_stmt()
    }

    fn print_stmt(&mut self) -> Result<Stmt, LoxError> {
        let expr = self.expression()?;
        self.expect(TokenType::Semicolon, ";", "a print statement")?;
        Ok(Stmt::Print(Rc::new(expr)))
    }

    fn block_stmt(&mut self) -> Result<Stmt, LoxError> {
        let mut statements: Vec<Rc<Stmt>> = vec![];
        while !self.is_type(TokenType::RightBrace) && !self.is_end() {
            let stmt = self.declaration()?;
            statements.push(Rc::new(stmt));
        }
        self.expect(TokenType::RightBrace, "}", "block")?;
        Ok(Stmt::Block(statements))
    }

    fn if_stmt(&mut self) -> Result<Stmt, LoxError> {
        self.expect(TokenType::LeftParen, "(", "'if'")?;
        let condition = self.expression()?;
        self.expect(TokenType::RightParen, ")", "'if' condition")?;

        let then_branch = self.statement()?;
        let else_branch = match self.advance_maybe(&[TokenType::Else]) {
            true => {
                let else_stmt = self.statement()?;
                Some(Rc::new(else_stmt))
            }
            false => None,
        };

        Ok(Stmt::If(Rc::new(condition), Rc::new(then_branch), else_branch))
    }

    fn while_stmt(&mut self) -> Result<Stmt, LoxError> {
        self.expect(TokenType::LeftParen, "(", "'while'")?;
        let condition = self.expression()?;
        self.expect(TokenType::RightParen, ")", "'while' condition")?;
        let stmts = self.statement()?;
        Ok(Stmt::While(Rc::new(condition), Rc::new(stmts)))
    }

    fn for_stmt(&mut self) -> Result<Stmt, LoxError> {
        self.expect(TokenType::LeftParen, "(", "'for'")?;

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
        self.expect(TokenType::Semicolon, ";", "for-loop condition")?;

        // Parse increment
        let orig_inc = match self.is_type(TokenType::RightParen) {
            true => None,
            false => Some(self.expression()?),
        };
        self.expect(TokenType::RightParen, ")", "for-loop clauses")?;

        // Parse statement
        let orig_body = self.statement()?;

        // if it exists, add the increment to the end of the while-body
        let body = match orig_inc {
            None => orig_body,
            Some(inc) => Stmt::Block(vec![Rc::new(orig_body), Rc::new(Stmt::Expression(Rc::new(inc)))]),
        };

        // if not explicitly provided, set the condition to perma-true
        let cond = match orig_cond {
            Some(expr) => expr,
            None => {
                let fake_token = Token::new(TokenType::True, Lit::Bool(true), self.peek().get_position());
                Expr::Literal(Rc::new(fake_token))
            }
        };

        // if it exists, add the initializer before the while-body
        let result = match orig_init {
            None => Stmt::While(Rc::new(cond), Rc::new(body)),
            Some(init) => Stmt::Block(vec![Rc::new(init), Rc::new(Stmt::While(Rc::new(cond), Rc::new(body)))]),
        };
        Ok(result)
    }

    fn expr_stmt(&mut self) -> Result<Stmt, LoxError> {
        let expr = self.expression()?;
        self.expect(TokenType::Semicolon, ";", "an expression statement")?;
        Ok(Stmt::Expression(Rc::new(expr)))
    }

    fn return_stmt(&mut self) -> Result<Stmt, LoxError> {
        let token = self.previous();
        let expr: Option<Rc<Expr>> = match self.is_type(TokenType::Semicolon) {
            true => None,
            false => Some(Rc::new(self.expression()?)),
        };
        self.expect(TokenType::Semicolon, ";", "return statement")?;
        Ok(Stmt::Return(Rc::clone(&token), expr))
    }

    fn expression(&mut self) -> Result<Expr, LoxError> {
        self.assign_expr()
    }

    fn assign_expr(&mut self) -> Result<Expr, LoxError> {
        let expr = self.or_expr()?;

        if self.advance_maybe(&[TokenType::Assign]) {
            let eq_token = self.previous();
            return match expr {
                Expr::Variable(name) => {
                    let value = self.assign_expr()?;
                    Ok(Expr::Assign(name, Rc::new(value)))
                }
                _ => Err(LoxError::invalid_assignment(eq_token.get_position(), &expr)),
            };
        }

        Ok(expr)
    }

    fn or_expr(&mut self) -> Result<Expr, LoxError> {
        let mut expr = self.and_expr()?;

        while self.advance_maybe(&[TokenType::Or]) {
            let operator = self.previous();
            let right = self.and_expr()?;
            expr = Expr::Logical(Rc::new(expr), operator, Rc::new(right));
        }

        Ok(expr)
    }

    fn and_expr(&mut self) -> Result<Expr, LoxError> {
        let mut expr = self.equality_expr()?;

        while self.advance_maybe(&[TokenType::And]) {
            let operator = self.previous();
            let right = self.equality_expr()?;
            expr = Expr::Logical(Rc::new(expr), operator, Rc::new(right));
        }

        Ok(expr)
    }

    fn equality_expr(&mut self) -> Result<Expr, LoxError> {
        let mut expr: Expr = self.comparison_expr()?;
        while self.advance_maybe(&[TokenType::Neq, TokenType::Eq]) {
            let token = self.previous();
            let right = self.comparison_expr()?;
            expr = Expr::Binary(Rc::new(expr), token, Rc::new(right));
        }
        Ok(expr)
    }

    fn comparison_expr(&mut self) -> Result<Expr, LoxError> {
        let mut expr = self.term_expr()?;

        while self.advance_maybe(&[TokenType::Gt, TokenType::Geq, TokenType::Lt, TokenType::Leq]) {
            let token = self.previous();
            let right = self.term_expr()?;
            expr = Expr::Binary(Rc::new(expr), token, Rc::new(right));
        }
        Ok(expr)
    }

    fn term_expr(&mut self) -> Result<Expr, LoxError> {
        let mut expr = self.factor_expr()?;

        while self.advance_maybe(&[TokenType::Minus, TokenType::Plus]) {
            let token = self.previous();
            let right = self.factor_expr()?;
            expr = Expr::Binary(Rc::new(expr), token, Rc::new(right));
        }
        Ok(expr)
    }

    fn factor_expr(&mut self) -> Result<Expr, LoxError> {
        let mut expr = self.unary_expr()?;

        while self.advance_maybe(&[TokenType::Slash, TokenType::Star]) {
            let token = self.previous();
            let right = self.unary_expr()?;
            expr = Expr::Binary(Rc::new(expr), token, Rc::new(right));
        }
        Ok(expr)
    }

    fn unary_expr(&mut self) -> Result<Expr, LoxError> {
        if self.advance_maybe(&[TokenType::Bang, TokenType::Minus]) {
            let token = self.previous();
            let right = self.unary_expr()?;
            return Ok(Expr::Unary(token, Rc::new(right)));
        }
        self.call_expr()
    }

    fn call_expr(&mut self) -> Result<Expr, LoxError> {
        let mut expr = self.primary_expr()?;

        while self.advance_maybe(&[TokenType::LeftParen]) {
            let mut args: Vec<Expr> = vec![];
            if !self.is_type(TokenType::RightParen) {
                loop {
                    args.push(self.expression()?);
                    if args.len() >= 255 {
                        return Err(LoxError::too_many_args(self.peek().get_position()));
                    }
                    if !self.advance_maybe(&[TokenType::Comma]) {
                        break;
                    }
                }
            }
            let paren = self.expect(TokenType::RightParen, ")", "function call")?;
            expr = Expr::Call(Rc::new(expr), paren, args);
        }
        Ok(expr)
    }

    fn primary_expr(&mut self) -> Result<Expr, LoxError> {
        let token = self.peek();
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
                self.expect(TokenType::RightParen, ")", "grouping expression")?;
                Ok(Expr::Grouping(Rc::new(expr)))
            }
            _ => {
                let message = format!("Expected expression at '{}'", self.peek().get_lexeme());
                Err(LoxError::Syntax(self.peek().get_position(), message))
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

    fn expect(&mut self, tt: TokenType, exp: &str, after: &str) -> Result<Rc<Token>, LoxError> {
        if self.is_type(tt) {
            return Ok(self.advance());
        }

        let token = if self.is_end() { self.previous() } else { self.peek() };
        let pos = token.get_position();
        let lexeme = token.get_lexeme();

        Err(LoxError::expected(pos, exp, after, &lexeme))
    }

    fn advance(&mut self) -> Rc<Token> {
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

    fn peek(&self) -> Rc<Token> {
        Rc::clone(self.tokens.get(self.current).unwrap())
    }

    fn previous(&self) -> Rc<Token> {
        Rc::clone(self.tokens.get(self.current - 1).unwrap())
    }

    fn is_end(&self) -> bool {
        if let TokenType::EndOfFile = self.peek().get_type() {
            return true;
        }
        false
    }
}
