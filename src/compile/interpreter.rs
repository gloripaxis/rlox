use std::{cell::RefCell, rc::Rc};

use super::env::Environment;
use crate::{
    errors::LoxError,
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
        let value = right.accept(self)?;
        match op.get_type() {
            TokenType::Minus => match value {
                Lit::Num(x) => Ok(Lit::Num(-x)),
                _ => Err(LoxError::unary_operand(op.get_position(), op.get_type(), &value)),
            },
            TokenType::Bang => Ok(Lit::Bool(value.is_truthy())),
            x => unreachable!("ENCOUNTERED INVALID UNARY OPERATOR: {x}",),
        }
    }

    fn visit_binary_expr(&mut self, left: &Expr, op: &Token, right: &Expr) -> Result<Lit, LoxError> {
        let lval = left.accept(self)?;
        let rval = right.accept(self)?;
        let typ = op.get_type();
        let pos = op.get_position();

        match op.get_type() {
            TokenType::Minus => match (&lval, &rval) {
                (Lit::Num(l), Lit::Num(r)) => Ok(Lit::Num(l - r)),
                _ => Err(LoxError::binary_operands(pos, typ, &lval, &rval)),
            },
            TokenType::Plus => match (&lval, &rval) {
                (Lit::Num(l), Lit::Num(r)) => Ok(Lit::Num(l + r)),
                (Lit::Str(l), Lit::Str(r)) => Ok(Lit::Str(Rc::from(format!("{l}{r}")))),
                _ => Err(LoxError::plus_operands(pos, &lval, &rval)),
            },
            TokenType::Slash => match (&lval, &rval) {
                (Lit::Num(l), Lit::Num(r)) => Ok(Lit::Num(l / r)),
                _ => Err(LoxError::binary_operands(pos, typ, &lval, &rval)),
            },
            TokenType::Star => match (&lval, &rval) {
                (Lit::Num(l), Lit::Num(r)) => Ok(Lit::Num(l * r)),
                _ => Err(LoxError::binary_operands(pos, typ, &lval, &rval)),
            },
            TokenType::Gt => match (&lval, &rval) {
                (Lit::Num(l), Lit::Num(r)) => Ok(Lit::Bool(l > r)),
                _ => Err(LoxError::binary_operands(pos, typ, &lval, &rval)),
            },
            TokenType::Geq => match (&lval, &rval) {
                (Lit::Num(l), Lit::Num(r)) => Ok(Lit::Bool(l >= r)),
                _ => Err(LoxError::binary_operands(pos, typ, &lval, &rval)),
            },
            TokenType::Lt => match (&lval, &rval) {
                (Lit::Num(l), Lit::Num(r)) => Ok(Lit::Bool(l < r)),
                _ => Err(LoxError::binary_operands(pos, typ, &lval, &rval)),
            },
            TokenType::Leq => match (&lval, &rval) {
                (Lit::Num(l), Lit::Num(r)) => Ok(Lit::Bool(l <= r)),
                _ => Err(LoxError::binary_operands(pos, typ, &lval, &rval)),
            },
            TokenType::Eq => Ok(Lit::Bool(lval == rval)),
            TokenType::Neq => Ok(Lit::Bool(lval != rval)),
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

    pub fn interpret(&mut self, program: Vec<Stmt>) -> Result<(), Vec<LoxError>> {
        for stmt in program.iter() {
            if let Err(x) = stmt.accept(self) {
                return Err(vec![x]);
            }
        }
        Ok(())
    }
}
