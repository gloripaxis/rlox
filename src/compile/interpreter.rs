use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::env::Environment;
use crate::{
    errors::LoxError,
    types::{
        callable::{LoxCallable, LoxFunction},
        class::LoxClass,
        expression::Expr,
        statement::Stmt,
        token::{Token, TokenType},
        value::Val,
    },
    visitors::Visitor,
};

pub struct Interpreter {
    globals: Rc<RefCell<Environment>>,
    environment: Rc<RefCell<Environment>>,
    locals: HashMap<String, usize>,
}

impl Visitor<Val> for Interpreter {
    // --------------------- EXPRESSIONS ---------------------
    fn visit_unary_expr(&mut self, op: Rc<Token>, right: Rc<Expr>) -> Result<Val, LoxError> {
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

    fn visit_binary_expr(&mut self, left: Rc<Expr>, op: Rc<Token>, right: Rc<Expr>) -> Result<Val, LoxError> {
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

    fn visit_logic_expr(&mut self, left: Rc<Expr>, op: Rc<Token>, right: Rc<Expr>) -> Result<Val, LoxError> {
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

    fn visit_grouping_expr(&mut self, expr: Rc<Expr>) -> Result<Val, LoxError> {
        expr.accept(self)
    }

    fn visit_literal_expr(&mut self, value: Rc<Token>) -> Result<Val, LoxError> {
        Ok(Val::from_literal(&value.get_literal()))
    }

    fn visit_variable_expr(&mut self, name: Rc<Token>) -> Result<Val, LoxError> {
        self.lookup_variable(&name)
    }

    fn visit_assign_expr(&mut self, name: Rc<Token>, right: Rc<Expr>) -> Result<Val, LoxError> {
        let value = right.accept(self)?;

        let dist = self.locals.get(&name.get_id());
        if let Some(x) = dist {
            let env = self.get_env_ancestor(*x);
            env.borrow_mut().assign_here(&name, value.clone())?;
        } else {
            self.globals.borrow_mut().assign(&name, value.clone())?;
        }
        Ok(value)
    }

    fn visit_call_expr(&mut self, callee: Rc<Expr>, paren: Rc<Token>, args: &[Rc<Expr>]) -> Result<Val, LoxError> {
        let callee = callee.accept(self)?;
        match callee {
            Val::Func(callable) => {
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
            }
            Val::Class(callable) => {
                let mut arg_values: Vec<Val> = vec![];
                // hack: push Val::Class(LoxClass) as first argument to avoid cloning
                arg_values.push(Val::Class(Rc::clone(&callable)));
                for arg in args {
                    arg_values.push(arg.accept(self)?);
                }
                if arg_values.len() != callable.arity() {
                    return Err(LoxError::wrong_arity(
                        paren.get_position(),
                        &callable.name(),
                        callable.arity() - 1, // hack: -1 to account for fake first argument
                        arg_values.len() - 1, // hack: -1 to account for fake first argument
                    ));
                }
                callable.as_ref().call(self, arg_values)
            }
            _ => Err(LoxError::not_callable(paren.get_position(), &callee)),
        }
    }

    fn visit_get_expr(&mut self, object: Rc<Expr>, name: Rc<Token>) -> Result<Val, LoxError> {
        let value = object.accept(self)?;
        if let Val::Instance(x) = value {
            return x.borrow().get(name);
        }
        Err(LoxError::illegal_field_access(name.get_position(), &name.get_literal()))
    }

    fn visit_set_expr(&mut self, object: Rc<Expr>, name: Rc<Token>, value: Rc<Expr>) -> Result<Val, LoxError> {
        let instance = object.accept(self)?;
        if let Val::Instance(x) = instance {
            let val = value.accept(self)?;
            x.borrow_mut().set(name, val.clone());
            return Ok(val);
        }
        Err(LoxError::illegal_field_access(name.get_position(), &name.get_literal()))
    }

    // --------------------- STATEMENTS ---------------------
    fn visit_expression_stmt(&mut self, expr: Rc<Expr>) -> Result<(), LoxError> {
        expr.accept(self)?;
        Ok(())
    }

    fn visit_print_stmt(&mut self, expr: Rc<Expr>) -> Result<(), LoxError> {
        let value = expr.accept(self)?;
        println!("{value}");
        Ok(())
    }

    fn visit_var_stmt(&mut self, name: Rc<Token>, init: &Option<Rc<Expr>>) -> Result<(), LoxError> {
        let value = match init {
            Some(expr) => expr.accept(self)?,
            None => Val::Nil,
        };
        self.environment.borrow_mut().define(name.get_lexeme(), value);
        Ok(())
    }

    fn visit_block_stmt(&mut self, statements: &[Rc<Stmt>]) -> Result<(), LoxError> {
        let new_env = Environment::new(Some(Rc::clone(&self.environment)));
        self.execute_block(statements, new_env)
    }

    fn visit_if_stmt(&mut self, cond: Rc<Expr>, b_then: Rc<Stmt>, b_else: &Option<Rc<Stmt>>) -> Result<(), LoxError> {
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

    fn visit_while_stmt(&mut self, cond: Rc<Expr>, stmt: Rc<Stmt>) -> Result<(), LoxError> {
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
        let func = LoxFunction::new(Rc::clone(&name), vec_params, vec_body, Rc::clone(&self.environment));
        self.environment
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

    fn visit_class_stmt(&mut self, name: Rc<Token>, methods: &[Rc<Stmt>]) -> Result<(), LoxError> {
        self.environment.borrow_mut().define(name.get_lexeme(), Val::Nil);

        let mut method_map: HashMap<String, Rc<LoxFunction>> = HashMap::new();
        for method in methods.iter() {
            if let Stmt::Function(name, params, body) = method.as_ref() {
                let func = LoxFunction::new(
                    Rc::clone(name),
                    params.iter().map(Rc::clone).collect(),
                    body.iter().map(Rc::clone).collect(),
                    Rc::clone(&self.environment),
                );
                method_map.insert(name.get_lexeme(), Rc::new(func));
            }
        }

        let class = LoxClass::new(name.get_lexeme(), method_map);
        self.environment
            .borrow_mut()
            .assign(&name, Val::Class(Rc::new(class)))?;

        Ok(())
    }
}

impl Interpreter {
    pub fn new() -> Self {
        let global_env = Rc::new(RefCell::new(Environment::global()));
        Self {
            globals: Rc::clone(&global_env),
            environment: Rc::clone(&global_env),
            locals: HashMap::new(),
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
        let prev_env = Rc::clone(&self.environment);
        self.environment = Rc::new(RefCell::new(env));

        for stmt in statements.iter() {
            let result = stmt.accept(self);
            if let Err(x) = result {
                self.environment = prev_env;
                return Err(x);
            }
        }
        self.environment = prev_env;
        Ok(())
    }

    pub fn resolve(&mut self, var_id: String, depth: usize) {
        self.locals.insert(var_id, depth);
    }

    pub fn lookup_variable(&self, name: &Rc<Token>) -> Result<Val, LoxError> {
        let distance = self.locals.get(&name.get_id());
        if let Some(x) = distance {
            let env = self.get_env_ancestor(*x);
            return env.borrow().get_here(name);
        } else {
            return self.globals.borrow().get(name);
        }
    }

    fn get_env_ancestor(&self, x: usize) -> Rc<RefCell<Environment>> {
        let mut env = Rc::clone(&self.environment);
        if x == 0 {
            return env;
        }

        let mut i = x;
        while i > 0 {
            let tmp = env.borrow().get_parent().unwrap();
            env = tmp;
            i -= 1;
        }
        env
    }
}
