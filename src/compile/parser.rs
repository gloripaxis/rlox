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
                Ok(statement) => self.program.push(statement),
                Err(einfo) => self.errors.push(einfo),
            }
        }

        if !self.errors.is_empty() {
            Err(self.errors)
        } else {
            Ok(self.program)
        }
    }

    fn declaration(&mut self) -> Result<Rc<Stmt>, LoxError> {
        let result = if self.advance_maybe(&[TokenType::Fun]) {
            self.fun_decl("function")
        } else if self.advance_maybe(&[TokenType::Var]) {
            self.var_decl()
        } else if self.advance_maybe(&[TokenType::Class]) {
            self.class_decl()
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

    fn class_decl(&mut self) -> Result<Rc<Stmt>, LoxError> {
        let name = self.expect(TokenType::Identifier, "class name", "class")?;

        let mut superclass = None;
        if self.advance_maybe(&[TokenType::Lt]) {
            let name = self.expect(TokenType::Identifier, "superclass name", "<")?;
            superclass = Some(Rc::new(Expr::Variable(name)));
        }

        self.expect(TokenType::LeftBrace, "{", "class declaration")?;

        let mut methods: Vec<Rc<Stmt>> = vec![];
        while !self.is_type(TokenType::RightBrace) && !self.is_end() {
            methods.push(self.fun_decl("method")?);
        }

        self.expect(TokenType::RightBrace, "}", "class body")?;
        Ok(Rc::new(Stmt::Class(name, superclass, methods)))
    }

    fn fun_decl(&mut self, kind: &'static str) -> Result<Rc<Stmt>, LoxError> {
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

        // Destruture Block into Vec<Rc<Stmt>> to avoid block nesting
        if let Stmt::Block(stmts) = body.as_ref() {
            Ok(Rc::new(Stmt::Function(
                name,
                params,
                stmts.iter().map(Rc::clone).collect(),
            )))
        } else {
            unreachable!("Function body must always be a block statement");
        }
    }

    fn var_decl(&mut self) -> Result<Rc<Stmt>, LoxError> {
        let name = self.expect(TokenType::Identifier, "variable name", "'var'")?;
        let mut initializer: Option<Rc<Expr>> = None;
        if self.advance_maybe(&[TokenType::Assign]) {
            initializer = Some(self.expression()?);
        }
        self.expect(TokenType::Semicolon, ";", "variable declaration")?;
        Ok(Rc::new(Stmt::Var(name, initializer)))
    }

    fn statement(&mut self) -> Result<Rc<Stmt>, LoxError> {
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

    fn print_stmt(&mut self) -> Result<Rc<Stmt>, LoxError> {
        let expr = self.expression()?;
        self.expect(TokenType::Semicolon, ";", "a print statement")?;
        Ok(Rc::new(Stmt::Print(expr)))
    }

    fn block_stmt(&mut self) -> Result<Rc<Stmt>, LoxError> {
        let mut statements: Vec<Rc<Stmt>> = vec![];
        while !self.is_type(TokenType::RightBrace) && !self.is_end() {
            statements.push(self.declaration()?);
        }
        self.expect(TokenType::RightBrace, "}", "block")?;
        Ok(Rc::new(Stmt::Block(statements)))
    }

    fn if_stmt(&mut self) -> Result<Rc<Stmt>, LoxError> {
        self.expect(TokenType::LeftParen, "(", "'if'")?;
        let condition = self.expression()?;
        self.expect(TokenType::RightParen, ")", "'if' condition")?;

        let then_branch = self.statement()?;
        let else_branch = match self.advance_maybe(&[TokenType::Else]) {
            true => Some(self.statement()?),
            false => None,
        };

        Ok(Rc::new(Stmt::If(condition, then_branch, else_branch)))
    }

    fn while_stmt(&mut self) -> Result<Rc<Stmt>, LoxError> {
        self.expect(TokenType::LeftParen, "(", "'while'")?;
        let condition = self.expression()?;
        self.expect(TokenType::RightParen, ")", "'while' condition")?;
        let stmts = self.statement()?;
        Ok(Rc::new(Stmt::While(condition, stmts)))
    }

    fn for_stmt(&mut self) -> Result<Rc<Stmt>, LoxError> {
        self.expect(TokenType::LeftParen, "(", "'for'")?;

        // Parse initializer
        let orig_init: Option<Rc<Stmt>>;
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
            Some(inc) => Rc::new(Stmt::Block(vec![orig_body, Rc::new(Stmt::Expression(inc))])),
        };

        // if not explicitly provided, set the condition to perma-true
        let cond = match orig_cond {
            Some(expr) => expr,
            None => {
                let fake_token = Token::new(TokenType::True, Lit::Bool(true), self.peek().get_position());
                Rc::new(Expr::Literal(Rc::new(fake_token)))
            }
        };

        // if it exists, add the initializer before the while-body
        let result = match orig_init {
            None => Stmt::While(cond, body),
            Some(init) => Stmt::Block(vec![init, Rc::new(Stmt::While(cond, body))]),
        };
        Ok(Rc::new(result))
    }

    fn expr_stmt(&mut self) -> Result<Rc<Stmt>, LoxError> {
        let expr = self.expression()?;
        self.expect(TokenType::Semicolon, ";", "an expression statement")?;
        Ok(Rc::new(Stmt::Expression(expr)))
    }

    fn return_stmt(&mut self) -> Result<Rc<Stmt>, LoxError> {
        let token = self.previous();
        let expr: Option<Rc<Expr>> = match self.is_type(TokenType::Semicolon) {
            true => None,
            false => Some(self.expression()?),
        };
        self.expect(TokenType::Semicolon, ";", "return statement")?;
        Ok(Rc::new(Stmt::Return(token, expr)))
    }

    fn expression(&mut self) -> Result<Rc<Expr>, LoxError> {
        self.assign_expr()
    }

    fn assign_expr(&mut self) -> Result<Rc<Expr>, LoxError> {
        let expr = self.or_expr()?;

        if self.advance_maybe(&[TokenType::Assign]) {
            let eq_token = self.previous();
            let value = self.assign_expr()?;
            return match &*expr {
                Expr::Variable(name) => Ok(Rc::new(Expr::Assign(Rc::clone(name), value))),
                Expr::Get(object, name) => Ok(Rc::new(Expr::Set(Rc::clone(object), Rc::clone(name), value))),
                _ => Err(LoxError::invalid_assignment(eq_token.get_position(), &expr)),
            };
        }

        Ok(expr)
    }

    fn or_expr(&mut self) -> Result<Rc<Expr>, LoxError> {
        let mut expr = self.and_expr()?;

        while self.advance_maybe(&[TokenType::Or]) {
            let operator = self.previous();
            let right = self.and_expr()?;
            expr = Rc::new(Expr::Logical(expr, operator, right));
        }

        Ok(expr)
    }

    fn and_expr(&mut self) -> Result<Rc<Expr>, LoxError> {
        let mut expr = self.equality_expr()?;

        while self.advance_maybe(&[TokenType::And]) {
            let operator = self.previous();
            let right = self.equality_expr()?;
            expr = Rc::new(Expr::Logical(expr, operator, right));
        }

        Ok(expr)
    }

    fn equality_expr(&mut self) -> Result<Rc<Expr>, LoxError> {
        let mut expr = self.comparison_expr()?;
        while self.advance_maybe(&[TokenType::Neq, TokenType::Eq]) {
            let token = self.previous();
            let right = self.comparison_expr()?;
            expr = Rc::new(Expr::Binary(expr, token, right));
        }
        Ok(expr)
    }

    fn comparison_expr(&mut self) -> Result<Rc<Expr>, LoxError> {
        let mut expr = self.term_expr()?;

        while self.advance_maybe(&[TokenType::Gt, TokenType::Geq, TokenType::Lt, TokenType::Leq]) {
            let token = self.previous();
            let right = self.term_expr()?;
            expr = Rc::new(Expr::Binary(expr, token, right));
        }
        Ok(expr)
    }

    fn term_expr(&mut self) -> Result<Rc<Expr>, LoxError> {
        let mut expr = self.factor_expr()?;

        while self.advance_maybe(&[TokenType::Minus, TokenType::Plus]) {
            let token = self.previous();
            let right = self.factor_expr()?;
            expr = Rc::new(Expr::Binary(expr, token, right));
        }
        Ok(expr)
    }

    fn factor_expr(&mut self) -> Result<Rc<Expr>, LoxError> {
        let mut expr = self.unary_expr()?;

        while self.advance_maybe(&[TokenType::Slash, TokenType::Star]) {
            let token = self.previous();
            let right = self.unary_expr()?;
            expr = Rc::new(Expr::Binary(expr, token, right));
        }
        Ok(expr)
    }

    fn unary_expr(&mut self) -> Result<Rc<Expr>, LoxError> {
        if self.advance_maybe(&[TokenType::Bang, TokenType::Minus]) {
            let token = self.previous();
            let right = self.unary_expr()?;
            return Ok(Rc::new(Expr::Unary(token, right)));
        }
        self.call_expr()
    }

    fn call_expr(&mut self) -> Result<Rc<Expr>, LoxError> {
        let mut expr = self.primary_expr()?;

        loop {
            if self.advance_maybe(&[TokenType::LeftParen]) {
                expr = self.finish_call(expr)?;
            } else if self.advance_maybe(&[TokenType::Dot]) {
                let name = self.expect(TokenType::Identifier, "property name", ".")?;
                expr = Rc::new(Expr::Get(expr, name));
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn finish_call(&mut self, callee: Rc<Expr>) -> Result<Rc<Expr>, LoxError> {
        let mut args: Vec<Rc<Expr>> = vec![];
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
        Ok(Rc::new(Expr::Call(callee, paren, args)))
    }

    fn primary_expr(&mut self) -> Result<Rc<Expr>, LoxError> {
        let token = self.peek();
        match token.get_type() {
            TokenType::False | TokenType::True | TokenType::Nil | TokenType::Number | TokenType::String => {
                self.advance();
                Ok(Rc::new(Expr::Literal(token)))
            }
            TokenType::This => {
                self.advance();
                Ok(Rc::new(Expr::This(token)))
            }
            TokenType::Identifier => {
                self.advance();
                Ok(Rc::new(Expr::Variable(token)))
            }
            TokenType::LeftParen => {
                self.advance();
                let expr = self.expression()?;
                self.expect(TokenType::RightParen, ")", "grouping expression")?;
                Ok(Rc::new(Expr::Grouping(expr)))
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
