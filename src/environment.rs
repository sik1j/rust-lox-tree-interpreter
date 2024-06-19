use std::collections::HashMap;
use crate::parser::Expression;

#[derive(Debug)]
pub struct Environment {
    values: HashMap<String, Expression>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
        }
    }
    pub fn define(&mut self, name: &str, value: Option<Expression>) {
        let val = value.unwrap_or(Expression::Nil);
        self.values.insert(name.to_string(), val);
    }

    pub fn assign(&mut self, name: &str, value: Expression) -> Result<(), String> {
        if self.values.contains_key(name) {
            self.values.insert(name.to_string(), value);
            Ok(())
        } else {
            Err(format!("Undefined variable: {name}"))
        }
    }

    pub fn get(&self, name: &str) -> Expression {
        match self.values.get(name) {
            Some(val) => (*val).clone(),
            None => panic!("var '{}' not defined", name)
        }
    }
}