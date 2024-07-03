use crate::parser::Expression;
use std::collections::HashMap;

#[derive(Debug, Default)]
pub struct Environment {
    pub enclosing_environment: Option<Box<Environment>>,
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

    pub fn with_scope(enclosing_scope: Box<Environment>) -> Environment {
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

    pub fn assign(&mut self, name: &str, value: Expression) -> Result<(), String> {
        match (
            self.values.contains_key(name),
            &mut self.enclosing_environment,
        ) {
            (true, _) => {
                self.values.insert(name.to_string(), value);
            }
            (false, Some(enclosing_env)) => {
                enclosing_env.assign(name, value)?;
            }
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
