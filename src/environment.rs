use std::collections::HashMap;
use crate::parser::Expression;

#[derive(Debug)]
pub struct Environment<'a> {
    enclosing_environment: Option<&'a mut  Environment<'a>>,
    values: HashMap<String, Expression>,
}

impl Environment<'_> {
    pub fn without_scope() -> Self {
        Environment {
            enclosing_environment: None,
            values: HashMap::new(),
        }
    }

    pub fn with_scope<'a>(enclosing_scope: &'a mut Environment<'a>) -> Environment<'a> {
        Environment {
            enclosing_environment: Some(enclosing_scope),
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: &str, value: Option<Expression>) {
        let val = value.unwrap_or(Expression::Nil);
        self.values.insert(name.to_string(), val);
    }

    pub fn assign(&mut self, name: &str, value: Expression) -> Result<(), String> {
        match (self.values.contains_key(name), &mut self.enclosing_environment) {
            (true, _) => {self.values.insert(name.to_string(), value);},
            (false, Some(enclosing_env)) => { enclosing_env.assign(name, value)?;},
            (false, None) => return Err(format!("Undefined variable: {name}")),
        };
        Ok(())
    }

    pub fn get(&self, name: &str) -> Expression {
        match (self.values.get(name), &self.enclosing_environment) {
            (Some(val), _) => (*val).clone(),
            (None, None) => panic!("var '{}' not defined", name),
            (None, Some(enclosing_env)) => enclosing_env.get(name),
        }
    }
}