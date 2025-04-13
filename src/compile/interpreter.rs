use std::{cell::RefCell, rc::Rc};

use super::env::Environment;
use crate::{
    builtins::{clock::ClockFunction, read::ReadFunction},
    errors::LoxError,
    types::{
        callable::LoxFunction,
        expression::Expr,
        statement::Stmt,
        token::{Token, TokenType},
        value::Val,
    },
    visitors::Visitor,
};

pub struct Interpreter {
    env_stack: Vec<Rc<RefCell<Environment>>>,
}

impl Visitor<Val> for Interpreter {
    // --------------------- EXPRESSIONS ---------------------
    fn visit_unary_expr(&mut self, op: &Token, right: &Expr) -> Result<Val, LoxError> {
        let value = right.accept(self)?;
        match op.get_type() {
            TokenType::Minus => match value {
                Val::Num(x) => Ok(Val::Num(-x)),
                _ => Err(LoxError::unary_operand(op.get_position(), op.get_type(), &value)),
            },
            TokenType::Bang => Ok(Val::Bool(value.is_truthy())),
            x => unreachable!("ENCOUNTERED INVALID UNARY OPERATOR: {x}",),
        }
    }

    fn visit_binary_expr(&mut self, left: &Expr, op: &Token, right: &Expr) -> Result<Val, LoxError> {
        let lval = left.accept(self)?;
        let rval = right.accept(self)?;
        let typ = op.get_type();
        let pos = op.get_position();

        match op.get_type() {
            TokenType::Minus => match (&lval, &rval) {
                (Val::Num(l), Val::Num(r)) => Ok(Val::Num(l - r)),
                _ => Err(LoxError::binary_operands(pos, typ, &lval, &rval)),
            },
            TokenType::Plus => match (&lval, &rval) {
                (Val::Num(l), Val::Num(r)) => Ok(Val::Num(l + r)),
                (Val::Str(l), Val::Str(r)) => Ok(Val::Str(Rc::from(format!("{l}{r}")))),
                _ => Err(LoxError::plus_operands(pos, &lval, &rval)),
            },
            TokenType::Slash => match (&lval, &rval) {
                (Val::Num(l), Val::Num(r)) => Ok(Val::Num(l / r)),
                _ => Err(LoxError::binary_operands(pos, typ, &lval, &rval)),
            },
            TokenType::Star => match (&lval, &rval) {
                (Val::Num(l), Val::Num(r)) => Ok(Val::Num(l * r)),
                _ => Err(LoxError::binary_operands(pos, typ, &lval, &rval)),
            },
            TokenType::Gt => match (&lval, &rval) {
                (Val::Num(l), Val::Num(r)) => Ok(Val::Bool(l > r)),
                _ => Err(LoxError::binary_operands(pos, typ, &lval, &rval)),
            },
            TokenType::Geq => match (&lval, &rval) {
                (Val::Num(l), Val::Num(r)) => Ok(Val::Bool(l >= r)),
                _ => Err(LoxError::binary_operands(pos, typ, &lval, &rval)),
            },
            TokenType::Lt => match (&lval, &rval) {
                (Val::Num(l), Val::Num(r)) => Ok(Val::Bool(l < r)),
                _ => Err(LoxError::binary_operands(pos, typ, &lval, &rval)),
            },
            TokenType::Leq => match (&lval, &rval) {
                (Val::Num(l), Val::Num(r)) => Ok(Val::Bool(l <= r)),
                _ => Err(LoxError::binary_operands(pos, typ, &lval, &rval)),
            },
            TokenType::Eq => Ok(Val::Bool(lval == rval)),
            TokenType::Neq => Ok(Val::Bool(lval != rval)),
            x => unreachable!("ENCOUNTERED INVALID BINARY OPERATOR: {x}"),
        }
    }

    fn visit_logic_expr(&mut self, left: &Expr, op: &Token, right: &Expr) -> Result<Val, LoxError> {
        let left_val = left.accept(self)?;

        // NOTE: Original implementation returned the literal value of the operand, not strictly true or false
        // I find that disgusting and so decided to strictly return true or false :)
        let truthy = left_val.is_truthy();
        match op.get_type() {
            TokenType::Or if truthy => Ok(Val::Bool(true)),
            TokenType::And if !truthy => Ok(Val::Bool(false)),
            _ => {
                let val = right.accept(self)?;
                Ok(Val::Bool(val.is_truthy()))
            }
        }
    }

    fn visit_grouping_expr(&mut self, expr: &Expr) -> Result<Val, LoxError> {
        expr.accept(self)
    }

    fn visit_literal_expr(&mut self, value: &Token) -> Result<Val, LoxError> {
        Ok(Val::from_literal(&value.get_literal()))
    }

    fn visit_variable_expr(&mut self, name: &Token) -> Result<Val, LoxError> {
        self.env_stack.last().unwrap().borrow().get(name)
    }

    fn visit_assign_expr(&mut self, name: &Token, right: &Expr) -> Result<Val, LoxError> {
        let value = right.accept(self)?;
        self.env_stack
            .last()
            .unwrap()
            .borrow_mut()
            .assign(name, value.clone())?;
        Ok(value)
    }

    fn visit_call_expr(&mut self, callee: &Expr, paren: &Token, args: &[Expr]) -> Result<Val, LoxError> {
        let callee = callee.accept(self)?;
        if let Val::Func(callable) = callee {
            let mut arg_values: Vec<Val> = vec![];
            for arg in args {
                arg_values.push(arg.accept(self)?);
            }
            if arg_values.len() != callable.arity() {
                return Err(LoxError::wrong_arity(
                    paren.get_position(),
                    &callable.name(),
                    callable.arity(),
                    arg_values.len(),
                ));
            }
            callable.as_ref().call(self, arg_values)
        } else {
            Err(LoxError::not_callable(paren.get_position(), &callee))
        }
    }

    // --------------------- STATEMENTS ---------------------
    fn visit_expression_stmt(&mut self, expr: &Expr) -> Result<(), LoxError> {
        expr.accept(self)?;
        Ok(())
    }

    fn visit_print_stmt(&mut self, expr: &Expr) -> Result<(), LoxError> {
        let value = expr.accept(self)?;
        println!("{value}");
        Ok(())
    }

    fn visit_var_stmt(&mut self, name: &Token, init: &Option<Rc<Expr>>) -> Result<(), LoxError> {
        let value = match init {
            Some(expr) => expr.accept(self)?,
            None => Val::Nil,
        };
        self.env_stack
            .last()
            .unwrap()
            .borrow_mut()
            .define(name.get_lexeme(), value);
        Ok(())
    }

    fn visit_block_stmt(&mut self, statements: &[Rc<Stmt>]) -> Result<(), LoxError> {
        let new_env = Environment::new(Some(Rc::clone(self.env_stack.last().unwrap())));
        self.execute_block(statements, new_env)
    }

    fn visit_if_stmt(&mut self, cond: &Expr, b_then: &Stmt, b_else: &Option<Rc<Stmt>>) -> Result<(), LoxError> {
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

    fn visit_function_stmt(
        &mut self,
        name: Rc<Token>,
        params: &[Rc<Token>],
        body: &[Rc<Stmt>],
    ) -> Result<(), LoxError> {
        let vec_params: Vec<Rc<Token>> = params.iter().map(Rc::clone).collect();
        let vec_body: Vec<Rc<Stmt>> = body.iter().map(Rc::clone).collect();
        let func = LoxFunction::new(Rc::clone(&name), vec_params, vec_body);
        self.env_stack
            .last()
            .unwrap()
            .borrow_mut()
            .define(name.get_lexeme(), Val::Func(Rc::new(func)));
        Ok(())
    }

    fn visit_return_stmt(&mut self, _: Rc<Token>, expr: &Option<Rc<Expr>>) -> Result<(), LoxError> {
        let value = match expr {
            Some(ex) => ex.accept(self)?,
            None => Val::Nil,
        };
        Err(LoxError::Return(value))
    }
}

impl Interpreter {
    pub fn new() -> Self {
        let global_env = Rc::new(RefCell::new(Environment::new(None)));
        global_env
            .borrow_mut()
            .define(String::from("clock"), Val::Func(Rc::new(ClockFunction::new())));
        global_env
            .borrow_mut()
            .define(String::from("read"), Val::Func(Rc::new(ReadFunction::new())));
        Self {
            env_stack: vec![global_env],
        }
    }

    pub fn interpret(&mut self, program: Vec<Rc<Stmt>>) -> Result<(), Vec<LoxError>> {
        for stmt in program.iter() {
            if let Err(x) = stmt.accept(self) {
                return Err(vec![x]);
            }
        }
        Ok(())
    }

    pub fn execute_block(&mut self, statements: &[Rc<Stmt>], env: Environment) -> Result<(), LoxError> {
        self.env_stack.push(Rc::new(RefCell::new(env)));

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

    pub fn get_global_env(&self) -> Rc<RefCell<Environment>> {
        Rc::clone(self.env_stack.first().unwrap())
    }
}
