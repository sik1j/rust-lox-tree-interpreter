use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::parser::Expression;

#[derive(Debug, Default)]
pub struct Environment {
    pub enclosing_environment: Option<Rc<RefCell<Environment>>>,
    pub return_expr: Option<Expression>,
    values: HashMap<String, Expression>,
}

impl Environment {
    pub fn without_scope() -> Self {
        Environment {
            enclosing_environment: None,
            return_expr: None,
            values: HashMap::new(),
        }
    }

    pub fn with_scope(enclosing_scope: Rc<RefCell<Environment>>) -> Environment {
        Environment {
            enclosing_environment: Some(enclosing_scope),
            return_expr: None,
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: &str, value: Option<Expression>) {
        let val = value.unwrap_or(Expression::Nil);
        self.values.insert(name.to_string(), val);
    }
    fn assign(&mut self, name: &str, value: Expression) {
        self.values.insert(name.to_string(), value);
    }
    pub fn assign_at(&mut self, distance: &usize, name: &str, val: Expression) {
        if *distance == 0 {
            return self.assign(name, val);
        }
        return self
            .enclosing_environment
            .clone()
            .unwrap()
            .borrow_mut()
            .assign_at(&(distance - 1), name, val);
    }
    pub fn assign_global(&mut self, name: &str, val: Expression) {
        if let None = self.enclosing_environment {
            return self.assign(name, val);
        }
        return self
            .enclosing_environment
            .clone()
            .unwrap()
            .borrow_mut()
            .assign_global(name, val);
    }
    fn get(&self, name: &str) -> Expression {
        self.values
            .get(name)
            .expect(&format!("Variable {name} not defined"))
            .clone()
    }
    pub fn get_at(&self, distance: &usize, name: &str) -> Expression {
        if *distance == 0 {
            return self.get(name);
        }
        return self
            .enclosing_environment
            .clone()
            .unwrap()
            .borrow()
            .get_at(&(distance - 1), name);
    }

    pub fn get_global(&self, name: &str) -> Expression {
        if let None = self.enclosing_environment {
            return self.get(name);
        }
        return self
            .enclosing_environment
            .clone()
            .unwrap()
            .borrow()
            .get_global(name);
    }
}
