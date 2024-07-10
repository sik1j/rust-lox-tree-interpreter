use crate::interpreter::Interpreter;
use crate::parser::{Expression, FuncDecl, Statement, Variable};
use crate::scanner::Token;
use std::collections::HashMap;

pub struct Resolver {
    pub interpreter: Interpreter,
    scopes: Vec<HashMap<String, bool>>, // HashMap<variable name, is variable initialized?>
}

impl Resolver {
    pub fn new(interpreter: Interpreter) -> Self {
        Self {
            interpreter,
            scopes: vec![],
        }
    }
    pub fn resolve(&mut self, statements: &Vec<Statement>) {
        self.resolve_statements(statements);
    }
    pub fn resolve_statement(&mut self, statement: &Statement) {
        match statement {
            Statement::Print(expr) => self.resolve_expression(expr),
            Statement::Expression(expr) => self.resolve_expression(expr),
            Statement::Return(expr) => self.resolve_expression(expr),
            Statement::Function(decl) => self.resolve_function(decl),
            Statement::Block(statements) => self.resolve_block(statements),
            Statement::While { condition, body } => self.resolve_while(condition, body),
            Statement::If {
                condition,
                if_body,
                else_body,
            } => self.resolve_if(condition, if_body, else_body),
            Statement::VarDecl {
                identifier,
                initializer,
            } => self.resolve_var_decl(identifier, initializer),
        }
    }
    fn resolve_block(&mut self, statements: &Vec<Statement>) {
        self.begin_scope();
        self.resolve_statements(statements);
        self.end_scope();
    }
    fn resolve_function(&mut self, decl: &FuncDecl) {
        let FuncDecl { name, params, body } = decl;
        self.declare_name(name.lexeme.clone());
        self.define_name(name.lexeme.clone());

        self.resolve_function_body(decl);
    }
    fn resolve_function_body(&mut self, decl: &FuncDecl) {
        self.begin_scope();

        decl.params.iter().for_each(|param| {
            self.declare_name(param.lexeme.clone());
            self.define_name(param.lexeme.clone());
        });

        self.resolve_statements(&decl.body);
        self.end_scope()
    }
    fn resolve_while(&mut self, condition: &Expression, body: &Statement) {
        self.resolve_expression(condition);
        self.resolve_statement(body);
    }
    fn resolve_if(
        &mut self,
        condition: &Expression,
        if_body: &Statement,
        else_body: &Option<Box<Statement>>,
    ) {
        self.resolve_expression(condition);
        self.resolve_statement(if_body);
        if let Some(stmt) = else_body {
            self.resolve_statement(stmt);
        }
    }
    fn resolve_var_decl(&mut self, identifier: &Token, initializer: &Option<Expression>) {
        self.declare_name(identifier.lexeme.clone());

        if let Some(expr) = initializer {
            self.resolve_expression(expr);
        }

        self.define_name(identifier.lexeme.clone())
    }
    fn declare_name(&mut self, name: String) {
        if let Some(scope) = self.scopes.last_mut() {
            let None = scope.insert(name.clone(), false) else {
                panic!(
                    "variable '{name}' already exists in this scope. Lox does not support shadowing"
                );
            };
        };
    }
    fn define_name(&mut self, name: String) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, true);
        }
    }
    pub fn resolve_expression(&mut self, expression: &Expression) {
        match expression {
            Expression::Binary {
                left,
                operator,
                right,
            }
            | Expression::Logical {
                left,
                operator,
                right,
            } => self.resolve_binary(left, operator, right),
            Expression::Assignment { var, value } => self.resolve_assignment(var, value),
            Expression::Call {
                callee,
                arguments,
                closing_paren,
            } => self.resolve_call(callee, arguments, closing_paren),
            Expression::Unary { operator, operand } => self.resolve_expression(operand),
            Expression::Grouping(expr) => self.resolve_expression(expr),
            Expression::Variable(var) => self.resolve_variable(var),
            Expression::LoxFunction(_)
            | Expression::Number(_)
            | Expression::String(_)
            | Expression::Bool(_)
            | Expression::Nil => {}
        }
    }
    fn resolve_call(
        &mut self,
        callee: &Expression,
        arguments: &Vec<Expression>,
        closing_paren: &Token,
    ) {
        self.resolve_expression(callee);
        arguments
            .iter()
            .for_each(|expr| self.resolve_expression(expr));
    }
    fn resolve_binary(&mut self, left: &Expression, operator: &Token, right: &Expression) {
        self.resolve_expression(left);
        self.resolve_expression(right);
    }
    fn resolve_assignment(&mut self, var: &Variable, value: &Expression) {
        self.resolve_expression(value);
        self.resolve_local_var(var);
    }

    fn resolve_variable(&mut self, var: &Variable) {
        let Some(scope) = self.scopes.last_mut() else {
            // if scopes are empty, assume var global
            return;
        };

        if let Some(false) = scope.get(&var.identifier.lexeme) {
            panic!("Cannot read local variable in its own initializer.");
        }

        self.resolve_local_var(var);
    }

    fn resolve_local_var(&mut self, var: &Variable) {
        println!("resolving: {:?}", var);
        if let Some(scope_distance) = self
            .scopes
            .iter()
            .rev()
            .position(|scope| scope.get(&var.identifier.lexeme).is_some())
        {
            self.interpreter.resolve(var.id, scope_distance)
        };
    }

    pub fn resolve_statements(&mut self, statements: &Vec<Statement>) {
        statements
            .iter()
            .for_each(|statement| self.resolve_statement(statement));
    }
    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }
    fn end_scope(&mut self) {
        self.scopes.pop();
    }
}
