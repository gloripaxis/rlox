use std::{collections::HashMap, rc::Rc};

use crate::{
    errors::LoxError,
    types::{expression::Expr, statement::Stmt, token::Token},
    visitors::Visitor,
};

use super::interpreter::Interpreter;

#[derive(Debug, Copy, Clone)]
pub enum FunctionType {
    None,
    Function,
    Initializer,
    Method,
}

#[derive(Debug, Copy, Clone)]
pub enum ClassType {
    Class,
    None,
}

pub struct Resolver<'a> {
    interpreter: &'a mut Interpreter,
    scopes: Vec<HashMap<String, bool>>,
    ftype: FunctionType,
    ctype: ClassType,
}

impl<'a> Resolver<'a> {
    pub fn new(interpreter: &'a mut Interpreter) -> Self {
        Self {
            interpreter,
            scopes: vec![],
            ftype: FunctionType::None,
            ctype: ClassType::None,
        }
    }

    pub fn resolve(&mut self, statements: &[Rc<Stmt>]) -> Result<(), Vec<LoxError>> {
        if let Err(err) = self.resolve_program(statements) {
            return Err(vec![err]);
        }
        Ok(())
    }

    fn resolve_program(&mut self, statements: &[Rc<Stmt>]) -> Result<(), LoxError> {
        for stmt in statements.iter() {
            self.resolve_stmt(Rc::clone(stmt))?;
        }
        Ok(())
    }

    fn resolve_stmt(&mut self, stmt: Rc<Stmt>) -> Result<(), LoxError> {
        stmt.accept(self)
    }

    fn resolve_expr(&mut self, expr: Rc<Expr>) -> Result<(), LoxError> {
        expr.accept(self)
    }

    fn resolve_local(&mut self, name: Rc<Token>) -> Result<(), LoxError> {
        for i in (0..self.scopes.len()).rev() {
            if self.scopes.get(i).unwrap().contains_key(&name.get_lexeme()) {
                self.interpreter.resolve(name.get_id(), self.scopes.len() - 1 - i);
                return Ok(());
            }
        }
        // In case of global variables
        Ok(())
    }

    fn resolve_function_body(
        &mut self,
        params: &[Rc<Token>],
        body: &[Rc<Stmt>],
        ftype: FunctionType,
    ) -> Result<(), LoxError> {
        let enclosing = self.ftype;
        self.ftype = ftype;

        self.begin_scope();
        for param in params.iter() {
            self.declare(param)?;
            self.define(param);
        }
        self.resolve_program(body)?;
        self.end_scope();

        self.ftype = enclosing;
        Ok(())
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: &Rc<Token>) -> Result<(), LoxError> {
        if self.scopes.is_empty() {
            return Ok(());
        }
        let lex = name.get_lexeme();
        if self.scopes.last().unwrap().contains_key(&lex) {
            return Err(LoxError::redeclaring_in_scope(name.get_position(), &name.get_literal()));
        }
        self.scopes.last_mut().unwrap().insert(lex, false);
        Ok(())
    }

    fn define(&mut self, name: &Rc<Token>) {
        if self.scopes.is_empty() {
            return;
        }
        self.scopes.last_mut().unwrap().insert(name.get_lexeme(), true);
    }
}

impl Visitor<()> for Resolver<'_> {
    fn visit_unary_expr(&mut self, _: Rc<Token>, right: Rc<Expr>) -> Result<(), LoxError> {
        self.resolve_expr(right)
    }

    fn visit_binary_expr(&mut self, left: Rc<Expr>, _: Rc<Token>, right: Rc<Expr>) -> Result<(), LoxError> {
        self.resolve_expr(left)?;
        self.resolve_expr(right)
    }

    fn visit_logic_expr(&mut self, left: Rc<Expr>, _: Rc<Token>, right: Rc<Expr>) -> Result<(), LoxError> {
        self.resolve_expr(left)?;
        self.resolve_expr(right)
    }

    fn visit_grouping_expr(&mut self, expr: Rc<Expr>) -> Result<(), LoxError> {
        self.resolve_expr(expr)
    }

    fn visit_literal_expr(&mut self, _: Rc<Token>) -> Result<(), LoxError> {
        Ok(())
    }

    fn visit_variable_expr(&mut self, name: Rc<Token>) -> Result<(), LoxError> {
        if !self.scopes.is_empty() {
            if let Some(false) = self.scopes.last().unwrap().get(&name.get_lexeme()) {
                panic!("RAISE LOX ERROR");
            }
        }
        self.resolve_local(name)?;
        Ok(())
    }

    fn visit_assign_expr(&mut self, name: Rc<Token>, right: Rc<Expr>) -> Result<(), LoxError> {
        self.resolve_expr(right)?;
        self.resolve_local(name)
    }

    fn visit_call_expr(&mut self, callee: Rc<Expr>, _: Rc<Token>, args: &[Rc<Expr>]) -> Result<(), LoxError> {
        self.resolve_expr(callee)?;
        for ex in args.iter() {
            self.resolve_expr(Rc::clone(ex))?;
        }
        Ok(())
    }

    fn visit_get_expr(&mut self, object: Rc<Expr>, _: Rc<Token>) -> Result<(), LoxError> {
        self.resolve_expr(object)
    }

    fn visit_set_expr(&mut self, object: Rc<Expr>, _: Rc<Token>, value: Rc<Expr>) -> Result<(), LoxError> {
        self.resolve_expr(value)?;
        self.resolve_expr(object)
    }

    fn visit_this_expr(&mut self, token: Rc<Token>) -> Result<(), LoxError> {
        if let ClassType::None = self.ctype {
            return Err(LoxError::illegal_this(token.get_position()));
        }
        self.resolve_local(token)
    }

    fn visit_expression_stmt(&mut self, expr: Rc<Expr>) -> Result<(), LoxError> {
        self.resolve_expr(expr)
    }

    fn visit_print_stmt(&mut self, expr: Rc<Expr>) -> Result<(), LoxError> {
        self.resolve_expr(expr)
    }

    fn visit_var_stmt(&mut self, name: Rc<Token>, init: &Option<Rc<Expr>>) -> Result<(), LoxError> {
        self.declare(&name)?;
        if let Some(expr) = init {
            self.resolve_expr(Rc::clone(expr))?;
        }
        self.define(&name);
        Ok(())
    }

    fn visit_block_stmt(&mut self, statements: &[Rc<Stmt>]) -> Result<(), LoxError> {
        self.begin_scope();
        self.resolve_program(statements)?;
        self.end_scope();
        Ok(())
    }

    fn visit_if_stmt(&mut self, cond: Rc<Expr>, b_then: Rc<Stmt>, b_else: &Option<Rc<Stmt>>) -> Result<(), LoxError> {
        self.resolve_expr(cond)?;
        self.resolve_stmt(b_then)?;
        if let Some(else_stmt) = b_else {
            self.resolve_stmt(Rc::clone(else_stmt))?;
        }
        Ok(())
    }

    fn visit_while_stmt(&mut self, cond: Rc<Expr>, stmt: Rc<Stmt>) -> Result<(), LoxError> {
        self.resolve_expr(cond)?;
        self.resolve_stmt(stmt)
    }

    fn visit_function_stmt(
        &mut self,
        name: Rc<Token>,
        params: &[Rc<Token>],
        body: &[Rc<Stmt>],
    ) -> Result<(), LoxError> {
        self.declare(&name)?;
        self.define(&name);
        self.resolve_function_body(params, body, FunctionType::Function)
    }

    fn visit_return_stmt(&mut self, token: Rc<Token>, expr: &Option<Rc<Expr>>) -> Result<(), LoxError> {
        if let FunctionType::None = self.ftype {
            return Err(LoxError::global_return(token.get_position()));
        }

        if let Some(value) = expr {
            if let FunctionType::Initializer = self.ftype {
                return Err(LoxError::init_return(token.get_position()));
            }
            self.resolve_expr(Rc::clone(value))?;
        }
        Ok(())
    }

    fn visit_class_stmt(&mut self, name: Rc<Token>, methods: &[Rc<Stmt>]) -> Result<(), LoxError> {
        let enclosing = self.ctype;
        self.ctype = ClassType::Class;

        self.declare(&name)?;
        self.define(&name);

        self.begin_scope();
        self.scopes.last_mut().unwrap().insert(String::from("this"), true);

        for method in methods.iter() {
            if let Stmt::Function(name, params, body) = method.as_ref() {
                let ftype = match name.get_lexeme() == "init" {
                    true => FunctionType::Initializer,
                    false => FunctionType::Method,
                };
                self.resolve_function_body(params, body, ftype)?;
            } else {
                unreachable!("Statements within a class can only be methods");
            }
        }

        self.end_scope();
        self.ctype = enclosing;

        Ok(())
    }
}
