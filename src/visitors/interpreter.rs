use std::{cell::RefCell, error::Error, rc::Rc};

use crate::{
    env::Environment,
    errors::{ErrorMessage, ErrorType, RloxError},
    lexer::token::{Literal, Token, TokenType},
    parser::{expr::Expression, stmt::Statement},
};

pub struct Interpreter {
    env_stack: Vec<Rc<RefCell<Environment>>>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            env_stack: vec![Rc::new(RefCell::new(Environment::new(None)))],
        }
    }

    pub fn interpret(&mut self, program: Vec<Statement>) -> Result<(), Box<dyn Error>> {
        for stmt in program.iter() {
            self.execute(stmt)?;
        }
        Ok(())
    }

    fn execute(&mut self, stmt: &Statement) -> Result<(), Box<dyn Error>> {
        match stmt {
            Statement::Print(expr) => {
                let lit = self.evaluate(expr)?;
                println!("{lit}");
            }
            Statement::Expression(expr) => {
                self.evaluate(expr)?;
            }
            Statement::Var(token, initializer) => {
                let value = match initializer {
                    Some(x) => self.evaluate(x)?,
                    None => Literal::Nil,
                };
                self.env_stack
                    .last()
                    .unwrap()
                    .borrow_mut()
                    .define(String::from(token.get_lexeme()), value);
            }
            Statement::Block(stmts) => {
                let new_env = Environment::new(Some(Rc::clone(self.env_stack.last().unwrap())));
                self.env_stack.push(Rc::new(RefCell::new(new_env)));

                for stmt in stmts.iter() {
                    let result = self.execute(stmt);
                    if let Err(x) = result {
                        self.env_stack.pop();
                        return Err(x);
                    }
                }
                self.env_stack.pop();
            }
            Statement::If(cond, br_then, br_else) => {
                let value = self.evaluate(cond)?;
                match is_truthy(&value) {
                    true => self.execute(br_then)?,
                    false => {
                        if let Some(else_stmt) = br_else {
                            self.execute(else_stmt)?;
                        }
                    }
                };
            }
        }
        Ok(())
    }

    fn evaluate(&mut self, expr: &Expression) -> Result<Literal, Box<dyn Error>> {
        match expr {
            Expression::Literal(token) => self.eval_literal(token.get_literal()),
            Expression::Binary(left, token, right) => self.eval_binary(left, token, right),
            Expression::Grouping(expr) => self.evaluate(expr),
            Expression::Unary(token, right) => self.eval_unary(token, right),
            Expression::Variable(token) => self.env_stack.last().unwrap().borrow().get(token),
            Expression::Assign(token, value) => self.eval_assign(token, value),
            Expression::Logical(left, token, right) => self.eval_logic(left, token, right),
        }
    }

    fn eval_literal(&self, literal: Literal) -> Result<Literal, Box<dyn Error>> {
        Ok(literal)
    }

    fn eval_binary(&mut self, left: &Expression, token: &Token, right: &Expression) -> Result<Literal, Box<dyn Error>> {
        let left_lit = self.evaluate(left)?;
        let right_lit = self.evaluate(right)?;

        match token.get_type() {
            TokenType::Minus => match (&left_lit, &right_lit) {
                (Literal::Number(l), Literal::Number(r)) => Ok(Literal::Number(l - r)),
                _ => Err(Box::new(binary_number_error(token, left_lit, right_lit))),
            },
            TokenType::Plus => match (&left_lit, &right_lit) {
                (Literal::Number(l), Literal::Number(r)) => Ok(Literal::Number(l + r)),
                (Literal::String(l), Literal::String(r)) => Ok(Literal::String(format!("{l}{r}"))),
                _ => Err(Box::new(plus_error(token, left_lit, right_lit))),
            },
            TokenType::Slash => match (&left_lit, &right_lit) {
                (Literal::Number(l), Literal::Number(r)) => Ok(Literal::Number(l / r)),
                _ => Err(Box::new(binary_number_error(token, left_lit, right_lit))),
            },
            TokenType::Star => match (&left_lit, &right_lit) {
                (Literal::Number(l), Literal::Number(r)) => Ok(Literal::Number(l * r)),
                _ => Err(Box::new(binary_number_error(token, left_lit, right_lit))),
            },
            TokenType::Gt => match (&left_lit, &right_lit) {
                (Literal::Number(l), Literal::Number(r)) => Ok(Literal::Boolean(l > r)),
                _ => Err(Box::new(binary_number_error(token, left_lit, right_lit))),
            },
            TokenType::Geq => match (&left_lit, &right_lit) {
                (Literal::Number(l), Literal::Number(r)) => Ok(Literal::Boolean(l >= r)),
                _ => Err(Box::new(binary_number_error(token, left_lit, right_lit))),
            },
            TokenType::Lt => match (&left_lit, &right_lit) {
                (Literal::Number(l), Literal::Number(r)) => Ok(Literal::Boolean(l < r)),
                _ => Err(Box::new(binary_number_error(token, left_lit, right_lit))),
            },
            TokenType::Leq => match (&left_lit, &right_lit) {
                (Literal::Number(l), Literal::Number(r)) => Ok(Literal::Boolean(l <= r)),
                _ => Err(Box::new(binary_number_error(token, left_lit, right_lit))),
            },
            TokenType::Eq => Ok(Literal::Boolean(is_equal(left_lit, right_lit))),
            TokenType::Neq => Ok(Literal::Boolean(!is_equal(left_lit, right_lit))),
            _ => Err(Box::new(invalid_binary_operator(token))),
        }
    }

    fn eval_unary(&mut self, token: &Token, right_expr: &Expression) -> Result<Literal, Box<dyn Error>> {
        let right = self.evaluate(right_expr)?;
        match token.get_type() {
            TokenType::Minus => match right {
                Literal::Number(x) => Ok(Literal::Number(-x)),
                _ => Err(Box::new(unary_number_error(token, right))),
            },
            TokenType::Bang => Ok(Literal::Boolean(!is_truthy(&right))),
            _ => Err(Box::new(invalid_unary_operator(token))),
        }
    }

    fn eval_assign(&mut self, token: &Token, value: &Expression) -> Result<Literal, Box<dyn Error>> {
        let lit = self.evaluate(value)?;
        self.env_stack.last().unwrap().borrow_mut().assign(token, lit.clone())?;
        Ok(lit)
    }

    fn eval_logic(&mut self, left: &Expression, token: &Token, right: &Expression) -> Result<Literal, Box<dyn Error>> {
        let left_val = self.evaluate(left)?;

        // NOTE: Original implementation returned the literal value of the operand, not strictly true or false
        // I find that disgusting and so decided to strictly return true or false :)
        let truthy = is_truthy(&left_val);
        match token.get_type() {
            TokenType::Or if truthy => Ok(Literal::Boolean(true)),
            TokenType::And if !truthy => Ok(Literal::Boolean(false)),
            _ => {
                let val = self.evaluate(right)?;
                Ok(Literal::Boolean(is_truthy(&val)))
            }
        }
    }
}

fn is_truthy(literal: &Literal) -> bool {
    match literal {
        Literal::Nil => false,
        Literal::Boolean(x) => *x,
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

fn unary_number_error(op_token: &Token, right: Literal) -> RloxError {
    let (line, column) = op_token.get_location();
    let emsg = ErrorMessage::new(
        ErrorType::Runtime,
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

fn binary_number_error(op_token: &Token, left: Literal, right: Literal) -> RloxError {
    let (line, column) = op_token.get_location();
    let emsg = ErrorMessage::new(
        ErrorType::Runtime,
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

fn plus_error(op_token: &Token, left: Literal, right: Literal) -> RloxError {
    let (line, column) = op_token.get_location();
    let emsg = ErrorMessage::new(
        ErrorType::Runtime,
        format!(
            "Operands of + must be either both strings or both numbers. Found {:?} and {:?}",
            left, right
        ),
        line,
        column,
    );
    RloxError::new(vec![emsg])
}

fn invalid_binary_operator(op_token: &Token) -> RloxError {
    let (line, column) = op_token.get_location();
    let emsg = ErrorMessage::new(
        ErrorType::Runtime,
        format!("(CRITICAL) Invalid binary operator detected: {}", op_token.get_lexeme()),
        line,
        column,
    );
    RloxError::new(vec![emsg])
}

fn invalid_unary_operator(op_token: &Token) -> RloxError {
    let (line, column) = op_token.get_location();
    let emsg = ErrorMessage::new(
        ErrorType::Runtime,
        format!("(CRITICAL) Invalid unnary operator detected: {}", op_token.get_lexeme()),
        line,
        column,
    );
    RloxError::new(vec![emsg])
}
