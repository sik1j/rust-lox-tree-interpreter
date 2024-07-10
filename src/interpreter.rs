use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::environment::Environment;
use crate::parser::{Expression, FuncDecl};
use crate::parser::{Statement, Variable};
use crate::scanner::{Token, TokenType};

#[derive(Debug, Default)]
pub struct Interpreter {
    environment: Rc<RefCell<Environment>>,
    locals_vars: HashMap<u64, usize>,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            environment: Rc::new(RefCell::new(Environment::without_scope())),
            locals_vars: HashMap::new(),
        }
    }
    pub fn resolve(&mut self, var_id: u64, scope_distance: usize) {
        println!("putting in: {var_id} at {scope_distance}");
        self.locals_vars.insert(var_id, scope_distance);
    }
    fn look_up_var(&self, var: &Variable) -> Expression {
        match self.locals_vars.get(&var.id) {
            Some(distance) => self
                .environment
                .borrow()
                .get_at(distance, &var.identifier.lexeme),
            None => self.environment.borrow().get_global(&var.identifier.lexeme),
        }
    }
    fn assign_var(&mut self, var: &Variable, val: Expression) {
        match self.locals_vars.get(&var.id) {
            None => self
                .environment
                .borrow_mut()
                .assign_global(&var.identifier.lexeme, val),
            Some(distance) => {
                self.environment
                    .borrow_mut()
                    .assign_at(distance, &var.identifier.lexeme, val)
            }
        }
    }

    pub fn interpret(&mut self, statements: Vec<Statement>) {
        for statement in statements {
            if let Err(msg) = self.execute(&statement) {
                println!("{}", msg);
            }
        }
    }
    fn evaluate(&mut self, expr: &Expression) -> Result<Expression, String> {
        match expr {
            Expression::Binary {
                left,
                operator,
                right,
            } => self.eval_binary(left, operator, right),
            Expression::Unary { operator, operand } => self.eval_unary(operator, operand),
            Expression::Grouping(expr) => self.evaluate(expr),
            Expression::Variable(var) => {
                let val = self.look_up_var(var);
                Ok(val.clone())
            }
            Expression::Assignment {
                var,
                value: expression,
            } => {
                let val = self.evaluate(expression)?;
                self.assign_var(var, val.clone());
                Ok(val)
            }
            Expression::Logical {
                left,
                operator,
                right,
            } => self.eval_logical(left, operator, right),
            Expression::Call {
                callee,
                arguments,
                closing_paren,
            } => self.eval_func_call(callee, arguments, closing_paren),
            Expression::Number(_)
            | Expression::String(_)
            | Expression::Bool(_)
            | Expression::LoxFunction(_)
            | Expression::Nil => Ok(expr.clone()),
        }
    }
    fn eval_func_call(
        &mut self,
        callee: &Expression,
        arguments: &Vec<Expression>,
        closing_paren: &Token,
    ) -> Result<Expression, String> {
        let res = self.evaluate(callee)?;
        let Expression::LoxFunction(mut function) = res else {
            panic!("Cannot call {:?}", res);
        };

        let arity = function.arity();
        let arg_len = arguments.len();

        if arity != arg_len {
            panic!("Expected {arity} arguments, received {arg_len}",)
        }

        let mut evald_args = vec![];
        for argument in arguments {
            evald_args.push(self.evaluate(&argument)?);
        }

        function.call(self, evald_args);
        Ok(
            std::mem::take(&mut self.environment.borrow_mut().return_expr)
                .unwrap_or_else(|| Expression::Nil),
        )
    }

    fn eval_binary(
        &mut self,
        lhs: &Expression,
        op: &Token,
        rhs: &Expression,
    ) -> Result<Expression, String> {
        let lhs = self.evaluate(lhs)?;
        let rhs = self.evaluate(rhs)?;

        Ok(match &op.token_type {
            TokenType::EqualEqual => Expression::Bool(Self::is_equal(&lhs, &rhs)),
            TokenType::BangEqual => Expression::Bool(!Self::is_equal(&lhs, &rhs)),
            _ => match (&lhs, &rhs) {
                (Expression::Number(lhs), Expression::Number(rhs)) => match &op.token_type {
                    TokenType::Plus => Expression::Number(lhs + rhs),
                    TokenType::Minus => Expression::Number(lhs - rhs),
                    TokenType::Star => Expression::Number(lhs * rhs),
                    TokenType::Slash => Expression::Number(lhs / rhs),
                    TokenType::Greater => Expression::Bool(lhs > rhs),
                    TokenType::GreaterEqual => Expression::Bool(lhs >= rhs),
                    TokenType::Less => Expression::Bool(lhs < rhs),
                    TokenType::LessEqual => Expression::Bool(lhs <= rhs),
                    _ => return Err(format!("Cannot perform '{} {} {}'", lhs, op.lexeme, rhs)),
                },
                (Expression::String(lhs), Expression::String(rhs)) => {
                    Expression::String(match &op.token_type {
                        TokenType::Plus => format!("{}{}", lhs, rhs),
                        _ => return Err(format!("Cannot perform '{} {} {}'", lhs, op.lexeme, rhs)),
                    })
                }
                _ => {
                    return Err(format!(
                        "Cannot perform '{:?} {} {:?}'",
                        lhs, op.lexeme, rhs
                    ))
                }
            },
        })
    }

    fn eval_unary(&mut self, operator: &Token, operand: &Expression) -> Result<Expression, String> {
        let rhs = self.evaluate(operand)?;

        Ok(match operator.token_type {
            TokenType::Minus => match &rhs {
                Expression::Number(n) => Expression::Number(-n),
                _ => return Err(format!("Cannot apply '-' to {:?}", rhs)),
            },
            TokenType::Bang => Expression::Bool(!Self::is_truthy(&rhs)),
            _ => return Err(format!("Cannot apply '-' to {:?}", rhs)),
        })
    }

    fn is_truthy(expr: &Expression) -> bool {
        match expr {
            &Expression::Bool(b) => b,
            Expression::Nil => false,
            _ => true,
        }
    }

    fn is_equal(e1: &Expression, e2: &Expression) -> bool {
        match (e1, e2) {
            (Expression::Number(n), Expression::Number(m)) => n == m,
            (Expression::String(n), Expression::String(m)) => n == m,
            (Expression::Bool(n), Expression::Bool(m)) => n == m,
            (Expression::Nil, Expression::Nil) => true,
            _ => false,
        }
    }

    fn execute(&mut self, statement: &Statement) -> Result<(), String> {
        match statement {
            Statement::Print(expr) => {
                let val = self.evaluate(&expr)?;
                println!("{:?}", val);
            }
            Statement::Expression(expr) => {
                self.evaluate(&expr)?;
            }
            Statement::VarDecl {
                identifier,
                initializer,
            } => {
                let val;
                if let Some(expr) = initializer {
                    val = Some(self.evaluate(&expr)?);
                } else {
                    val = None;
                }

                self.environment
                    .borrow_mut()
                    .define(&identifier.lexeme, val);
            }
            Statement::Block(statements) => {
                let outer_env = self.environment.clone();
                let block_env = Rc::new(RefCell::new(Environment::with_scope(outer_env)));
                self.execute_block(statements, block_env)?
            }
            Statement::If {
                condition,
                if_body,
                else_body,
            } => {
                let val = self.evaluate(&condition)?;
                if Self::is_truthy(&val) {
                    self.execute(if_body)?;
                } else if let Some(branch) = else_body {
                    self.execute(branch)?;
                };
            }
            Statement::While { condition, body } => {
                while Self::is_truthy(&self.evaluate(&condition)?) {
                    self.execute(body)?;
                }
            }
            Statement::Function(decl) => {
                let name = decl.name.lexeme.clone();
                let function = LoxFunction::new(decl.clone(), self.environment.clone());
                self.environment
                    .borrow_mut()
                    .define(&name, Some(Expression::LoxFunction(function)));
            }
            Statement::Return(expr) => {
                let expr = self.evaluate(&expr)?;
                self.environment.borrow_mut().return_expr = Some(expr);
            }
        };
        Ok(())
    }
    pub fn execute_block(
        &mut self,
        statements: &Vec<Statement>,
        environment: Rc<RefCell<Environment>>,
    ) -> Result<(), String> {
        let previous_env = self.environment.clone();

        self.environment = environment;

        for statement in statements {
            if self.environment.borrow().return_expr.is_some() {
                break;
            }
            self.execute(statement)?;
        }

        let ret = std::mem::take(&mut self.environment.borrow_mut().return_expr);
        self.environment = previous_env;
        self.environment.borrow_mut().return_expr = ret;
        Ok(())
    }
    fn eval_logical(
        &mut self,
        lhs: &Expression,
        op: &Token,
        rhs: &Expression,
    ) -> Result<Expression, String> {
        let lhs_val = self.evaluate(lhs)?;

        match (&op.token_type, Self::is_truthy(&lhs_val)) {
            (TokenType::Or, true) | (TokenType::And, false) => Ok(lhs_val),
            (TokenType::Or, false) | (TokenType::And, true) => self.evaluate(rhs),
            (other, _) => panic!("Unexpected token {:?}", other),
        }
    }
}
#[derive(Clone, Debug)]
pub struct LoxFunction {
    pub declaration: FuncDecl,
    pub closure: Rc<RefCell<Environment>>,
}

impl LoxFunction {
    pub fn new(declaration: FuncDecl, closure: Rc<RefCell<Environment>>) -> Self {
        LoxFunction {
            declaration,
            closure,
        }
    }
    pub fn call(&mut self, interpreter: &mut Interpreter, mut arguments: Vec<Expression>) {
        let outer_env = self.closure.clone();
        let mut environment = Rc::new(RefCell::new(Environment::with_scope(outer_env)));

        for (param, arg) in self.declaration.params.iter().zip(arguments.iter_mut()) {
            environment
                .borrow_mut()
                .define(&param.lexeme, Some(std::mem::take(arg)))
        }

        interpreter
            .execute_block(&self.declaration.body, environment)
            .expect("TODO: panic message");
    }
    pub fn arity(&self) -> usize {
        self.declaration.params.len()
    }
}
