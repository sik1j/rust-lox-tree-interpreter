use std::collections::HashMap;
use crate::parser::Expression;

#[derive(Debug)]
pub struct Environment {
    values: HashMap<String, Option<Expression>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
        }
    }
    pub fn define(&mut self, name: &str, value: Option<Expression>) {
        self.values.insert(name.to_string(), value);
    }

    pub fn assign(&mut self, name: &str, value: Expression) -> Result<(), String> {
        if self.values.contains_key(name) {
            self.values.insert(name.to_string(), Some(value));
            Ok(())
        } else {
            Err(format!("Undefined variable: {name}"))
        }
    }

    pub fn get(&self, name: &str) -> Expression {
        match self.values.get(name) {
            Some(val) => match val {
                None => Expression::Nil,
                Some(val) => (*val).clone(),
            },
            None => panic!("var '{}' not defined", name)
        }
    }
}