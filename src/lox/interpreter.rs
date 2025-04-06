use std::{cell::RefCell, rc::Rc};

use super::env::Environment;
use crate::{
    errors::{ErrorInfo, LoxError},
    types::{
        expression::Expr,
        literal::Lit,
        statement::Stmt,
        token::{Token, TokenType},
    },
    visitors::Visitor,
};

pub struct Interpreter {
    env_stack: Vec<Rc<RefCell<Environment>>>,
}

impl Visitor<Lit> for Interpreter {
    // --------------------- EXPRESSIONS ---------------------
    fn visit_unary_expr(&mut self, op: &Token, right: &Expr) -> Result<Lit, LoxError> {
        let right_val = right.accept(self)?;
        match op.get_type() {
            TokenType::Minus => match right_val {
                Lit::Num(x) => Ok(Lit::Num(-x)),
                _ => Err(LoxError::Runtime(unary_number_error(op, right_val))),
            },
            TokenType::Bang => Ok(Lit::Bool(right_val.is_truthy())),
            x => unreachable!("ENCOUNTERED INVALID UNARY OPERATOR: {x}",),
        }
    }

    fn visit_binary_expr(&mut self, left: &Expr, op: &Token, right: &Expr) -> Result<Lit, LoxError> {
        let left_lit = left.accept(self)?;
        let right_lit = right.accept(self)?;

        match op.get_type() {
            TokenType::Minus => match (&left_lit, &right_lit) {
                (Lit::Num(l), Lit::Num(r)) => Ok(Lit::Num(l - r)),
                _ => Err(LoxError::Runtime(binary_number_error(op, left_lit, right_lit))),
            },
            TokenType::Plus => match (&left_lit, &right_lit) {
                (Lit::Num(l), Lit::Num(r)) => Ok(Lit::Num(l + r)),
                (Lit::Str(l), Lit::Str(r)) => Ok(Lit::Str(Rc::from(format!("{l}{r}")))),
                _ => Err(LoxError::Runtime(plus_error(op, left_lit, right_lit))),
            },
            TokenType::Slash => match (&left_lit, &right_lit) {
                (Lit::Num(l), Lit::Num(r)) => Ok(Lit::Num(l / r)),
                _ => Err(LoxError::Runtime(binary_number_error(op, left_lit, right_lit))),
            },
            TokenType::Star => match (&left_lit, &right_lit) {
                (Lit::Num(l), Lit::Num(r)) => Ok(Lit::Num(l * r)),
                _ => Err(LoxError::Runtime(binary_number_error(op, left_lit, right_lit))),
            },
            TokenType::Gt => match (&left_lit, &right_lit) {
                (Lit::Num(l), Lit::Num(r)) => Ok(Lit::Bool(l > r)),
                _ => Err(LoxError::Runtime(binary_number_error(op, left_lit, right_lit))),
            },
            TokenType::Geq => match (&left_lit, &right_lit) {
                (Lit::Num(l), Lit::Num(r)) => Ok(Lit::Bool(l >= r)),
                _ => Err(LoxError::Runtime(binary_number_error(op, left_lit, right_lit))),
            },
            TokenType::Lt => match (&left_lit, &right_lit) {
                (Lit::Num(l), Lit::Num(r)) => Ok(Lit::Bool(l < r)),
                _ => Err(LoxError::Runtime(binary_number_error(op, left_lit, right_lit))),
            },
            TokenType::Leq => match (&left_lit, &right_lit) {
                (Lit::Num(l), Lit::Num(r)) => Ok(Lit::Bool(l <= r)),
                _ => Err(LoxError::Runtime(binary_number_error(op, left_lit, right_lit))),
            },
            TokenType::Eq => Ok(Lit::Bool(left_lit == right_lit)),
            TokenType::Neq => Ok(Lit::Bool(left_lit != right_lit)),
            x => unreachable!("ENCOUNTERED INVALID BINARY OPERATOR: {x}"),
        }
    }

    fn visit_logic_expr(&mut self, left: &Expr, op: &Token, right: &Expr) -> Result<Lit, LoxError> {
        let left_val = left.accept(self)?;

        // NOTE: Original implementation returned the literal value of the operand, not strictly true or false
        // I find that disgusting and so decided to strictly return true or false :)
        let truthy = left_val.is_truthy();
        match op.get_type() {
            TokenType::Or if truthy => Ok(Lit::Bool(true)),
            TokenType::And if !truthy => Ok(Lit::Bool(false)),
            _ => {
                let val = right.accept(self)?;
                Ok(Lit::Bool(val.is_truthy()))
            }
        }
    }

    fn visit_grouping_expr(&mut self, expr: &Expr) -> Result<Lit, LoxError> {
        expr.accept(self)
    }

    fn visit_literal_expr(&mut self, value: &Token) -> Result<Lit, LoxError> {
        Ok(value.get_literal())
    }

    fn visit_variable_expr(&mut self, name: &Token) -> Result<Lit, LoxError> {
        self.env_stack.last().unwrap().borrow().get(name)
    }

    fn visit_assign_expr(&mut self, name: &Token, right: &Expr) -> Result<Lit, LoxError> {
        let lit = right.accept(self)?;
        self.env_stack.last().unwrap().borrow_mut().assign(name, lit.clone())?;
        Ok(lit)
    }

    // --------------------- STATEMENTS ---------------------
    fn visit_expression_stmt(&mut self, expr: &Expr) -> Result<(), LoxError> {
        expr.accept(self)?;
        Ok(())
    }

    fn visit_print_stmt(&mut self, expr: &Expr) -> Result<(), LoxError> {
        let lit = expr.accept(self)?;
        println!("{lit}");
        Ok(())
    }

    fn visit_var_stmt(&mut self, name: &Token, init: &Option<Expr>) -> Result<(), LoxError> {
        let value = match init {
            Some(expr) => expr.accept(self)?,
            None => Lit::Nil,
        };
        self.env_stack
            .last()
            .unwrap()
            .borrow_mut()
            .define(name.get_lexeme(), value);
        Ok(())
    }

    fn visit_block_stmt(&mut self, statements: &[Stmt]) -> Result<(), LoxError> {
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

    fn visit_if_stmt(&mut self, cond: &Expr, b_then: &Stmt, b_else: &Option<Box<Stmt>>) -> Result<(), LoxError> {
        let value = cond.accept(self)?;
        match value.is_truthy() {
            true => b_then.accept(self)?,
            false => {
                if let Some(else_stmt) = b_else {
                    else_stmt.accept(self)?;
                }
            }
        };
        Ok(())
    }

    fn visit_while_stmt(&mut self, cond: &Expr, stmt: &Stmt) -> Result<(), LoxError> {
        while cond.accept(self)?.is_truthy() {
            stmt.accept(self)?;
        }
        Ok(())
    }
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            env_stack: vec![Rc::new(RefCell::new(Environment::new(None)))],
        }
    }

    pub fn interpret(&mut self, program: Vec<Stmt>) -> Result<(), LoxError> {
        for stmt in program.iter() {
            stmt.accept(self)?;
        }
        Ok(())
    }
}

fn unary_number_error(op_token: &Token, right: Lit) -> ErrorInfo {
    let lexeme = op_token.get_lexeme();
    let msg = format!("Operand of '{}' must be a number; found {:?}", lexeme, right);
    ErrorInfo::from_token(op_token, msg)
}

fn binary_number_error(op_token: &Token, left: Lit, right: Lit) -> ErrorInfo {
    let lexeme = op_token.get_lexeme();
    let msg = format!(
        "Operands of '{}' must be numbers; found {:?} and {:?}",
        lexeme, left, right
    );
    ErrorInfo::from_token(op_token, msg)
}

fn plus_error(op_token: &Token, left: Lit, right: Lit) -> ErrorInfo {
    let msg = format!(
        "Operands of '+' must be both strings or both numbers; found {:?} and {:?}",
        left, right
    );
    ErrorInfo::from_token(op_token, msg)
}
