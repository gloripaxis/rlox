use std::{cell::RefCell, error::Error, rc::Rc};

use crate::{
    env::Environment,
    errors::{ErrorMessage, ErrorType, RloxError},
    lexer::token::{Literal, Token, TokenType},
    parser::{expr::Expression, stmt::Statement},
};

use super::Visitor;

pub struct Interpreter {
    env_stack: Vec<Rc<RefCell<Environment>>>,
}

impl Visitor<Literal> for Interpreter {
    fn visit_unary_expr(&mut self, op: &Token, right: &Expression) -> Result<Literal, Box<dyn Error>> {
        let right_val = right.accept(self)?;
        match op.get_type() {
            TokenType::Minus => match right_val {
                Literal::Number(x) => Ok(Literal::Number(-x)),
                _ => Err(Box::new(unary_number_error(op, right_val))),
            },
            TokenType::Bang => Ok(Literal::Boolean(!is_truthy(&right_val))),
            _ => Err(Box::new(invalid_unary_operator(op))),
        }
    }

    fn visit_binary_expr(
        &mut self,
        left: &Expression,
        op: &Token,
        right: &Expression,
    ) -> Result<Literal, Box<dyn Error>> {
        let left_lit = left.accept(self)?;
        let right_lit = right.accept(self)?;

        match op.get_type() {
            TokenType::Minus => match (&left_lit, &right_lit) {
                (Literal::Number(l), Literal::Number(r)) => Ok(Literal::Number(l - r)),
                _ => Err(Box::new(binary_number_error(op, left_lit, right_lit))),
            },
            TokenType::Plus => match (&left_lit, &right_lit) {
                (Literal::Number(l), Literal::Number(r)) => Ok(Literal::Number(l + r)),
                (Literal::String(l), Literal::String(r)) => Ok(Literal::String(format!("{l}{r}"))),
                _ => Err(Box::new(plus_error(op, left_lit, right_lit))),
            },
            TokenType::Slash => match (&left_lit, &right_lit) {
                (Literal::Number(l), Literal::Number(r)) => Ok(Literal::Number(l / r)),
                _ => Err(Box::new(binary_number_error(op, left_lit, right_lit))),
            },
            TokenType::Star => match (&left_lit, &right_lit) {
                (Literal::Number(l), Literal::Number(r)) => Ok(Literal::Number(l * r)),
                _ => Err(Box::new(binary_number_error(op, left_lit, right_lit))),
            },
            TokenType::Gt => match (&left_lit, &right_lit) {
                (Literal::Number(l), Literal::Number(r)) => Ok(Literal::Boolean(l > r)),
                _ => Err(Box::new(binary_number_error(op, left_lit, right_lit))),
            },
            TokenType::Geq => match (&left_lit, &right_lit) {
                (Literal::Number(l), Literal::Number(r)) => Ok(Literal::Boolean(l >= r)),
                _ => Err(Box::new(binary_number_error(op, left_lit, right_lit))),
            },
            TokenType::Lt => match (&left_lit, &right_lit) {
                (Literal::Number(l), Literal::Number(r)) => Ok(Literal::Boolean(l < r)),
                _ => Err(Box::new(binary_number_error(op, left_lit, right_lit))),
            },
            TokenType::Leq => match (&left_lit, &right_lit) {
                (Literal::Number(l), Literal::Number(r)) => Ok(Literal::Boolean(l <= r)),
                _ => Err(Box::new(binary_number_error(op, left_lit, right_lit))),
            },
            TokenType::Eq => Ok(Literal::Boolean(is_equal(left_lit, right_lit))),
            TokenType::Neq => Ok(Literal::Boolean(!is_equal(left_lit, right_lit))),
            _ => Err(Box::new(invalid_binary_operator(op))),
        }
    }

    fn visit_logic_expr(
        &mut self,
        left: &Expression,
        op: &Token,
        right: &Expression,
    ) -> Result<Literal, Box<dyn Error>> {
        let left_val = left.accept(self)?;

        // NOTE: Original implementation returned the literal value of the operand, not strictly true or false
        // I find that disgusting and so decided to strictly return true or false :)
        let truthy = is_truthy(&left_val);
        match op.get_type() {
            TokenType::Or if truthy => Ok(Literal::Boolean(true)),
            TokenType::And if !truthy => Ok(Literal::Boolean(false)),
            _ => {
                let val = right.accept(self)?;
                Ok(Literal::Boolean(is_truthy(&val)))
            }
        }
    }

    fn visit_grouping_expr(&mut self, expr: &Expression) -> Result<Literal, Box<dyn Error>> {
        expr.accept(self)
    }

    fn visit_literal_expr(&mut self, value: &Token) -> Result<Literal, Box<dyn Error>> {
        Ok(value.get_literal())
    }

    fn visit_variable_expr(&mut self, name: &Token) -> Result<Literal, Box<dyn Error>> {
        self.env_stack.last().unwrap().borrow().get(name)
    }

    fn visit_assign_expr(&mut self, name: &Token, right: &Expression) -> Result<Literal, Box<dyn Error>> {
        let lit = right.accept(self)?;
        self.env_stack.last().unwrap().borrow_mut().assign(name, lit.clone())?;
        Ok(lit)
    }

    fn visit_expression_stmt(&mut self, expr: &Expression) -> Result<(), Box<dyn Error>> {
        expr.accept(self)?;
        Ok(())
    }

    fn visit_print_stmt(&mut self, expr: &Expression) -> Result<(), Box<dyn Error>> {
        let lit = expr.accept(self)?;
        println!("{lit}");
        Ok(())
    }

    fn visit_var_stmt(&mut self, name: &Token, init: &Option<Expression>) -> Result<(), Box<dyn Error>> {
        let value = match init {
            Some(expr) => expr.accept(self)?,
            None => Literal::Nil,
        };
        self.env_stack
            .last()
            .unwrap()
            .borrow_mut()
            .define(String::from(name.get_lexeme()), value);
        Ok(())
    }

    fn visit_block_stmt(&mut self, statements: &[Statement]) -> Result<(), Box<dyn Error>> {
        let new_env = Environment::new(Some(Rc::clone(self.env_stack.last().unwrap())));
        self.env_stack.push(Rc::new(RefCell::new(new_env)));

        for stmt in statements.iter() {
            let result = stmt.accept(self);
            if let Err(x) = result {
                self.env_stack.pop();
                return Err(x);
            }
        }
        self.env_stack.pop();
        Ok(())
    }

    fn visit_if_stmt(
        &mut self,
        cond: &Expression,
        b_then: &Statement,
        b_else: &Option<Box<Statement>>,
    ) -> Result<(), Box<dyn Error>> {
        let value = cond.accept(self)?;
        match is_truthy(&value) {
            true => b_then.accept(self)?,
            false => {
                if let Some(else_stmt) = b_else {
                    else_stmt.accept(self)?;
                }
            }
        };
        Ok(())
    }
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            env_stack: vec![Rc::new(RefCell::new(Environment::new(None)))],
        }
    }

    pub fn interpret(&mut self, program: Vec<Statement>) -> Result<(), Box<dyn Error>> {
        for stmt in program.iter() {
            stmt.accept(self)?;
        }
        Ok(())
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
