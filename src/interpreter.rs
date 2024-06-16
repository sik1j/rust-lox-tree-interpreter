use crate::parser::Expression;
use crate::scanner::{Token, TokenType};

#[derive(Debug)]
pub struct Interpreter {}

impl Interpreter {
    pub fn interpret(expr: Expression) -> Result<Expression, String> {
        Self::evaluate(expr)
    }
    fn evaluate(expr: Expression) -> Result<Expression, String> {
        match expr {
            Expression::Binary { left, operator, right } => Self::eval_binary(*left, operator, *right),
            Expression::Unary { operator, operand } => Self::eval_unary(operator, *operand),
            Expression::Grouping(expr) => Self::evaluate(*expr),
            _ => Ok(expr)
        }
    }

    fn eval_binary(lhs: Expression, op: Token, rhs: Expression) -> Result<Expression, String> {
        let lhs = Self::evaluate(lhs)?;
        let rhs = Self::evaluate(rhs)?;

        Ok(match &op.token_type {
            TokenType::EqualEqual => Expression::Bool(Self::is_equal(&lhs, &rhs)),
            TokenType::BangEqual => Expression::Bool(!Self::is_equal(&lhs, &rhs)),
            _ =>
            match (&lhs, &rhs) {
                (Expression::Number(lhs), Expression::Number(rhs)) => match &op.token_type {
                    TokenType::Plus => Expression::Number(lhs + rhs),
                    TokenType::Minus => Expression::Number(lhs - rhs),
                    TokenType::Star => Expression::Number(lhs * rhs),
                    TokenType::Slash => Expression::Number(lhs / rhs),
                    TokenType::Greater => Expression::Bool(lhs > rhs),
                    TokenType::GreaterEqual => Expression::Bool(lhs >= rhs),
                    TokenType::Less => Expression::Bool(lhs < rhs),
                    TokenType::LessEqual => Expression::Bool(lhs <= rhs),
                    _ => return Err(format!("Cannot perform '{} {} {}'", lhs, op.lexeme, rhs))
                },
                (Expression::String(lhs), Expression::String(rhs)) => Expression::String(match &op.token_type {
                    TokenType::Plus => format!("{}{}", lhs, rhs),
                    _ => return Err(format!("Cannot perform '{} {} {}'", lhs, op.lexeme, rhs))
                }),
                _ => return Err(format!("Cannot perform '{:?} {} {:?}'", lhs, op.lexeme, rhs))
            }
        })

    }

    fn eval_unary(operator: Token, operand: Expression) -> Result<Expression, String> {
        let rhs = Self::evaluate(operand)?;

        Ok(match operator.token_type {
            TokenType::Minus => match &rhs {
                Expression::Number(n) => Expression::Number(-n),
                _ => return Err(format!("Cannot apply '-' to {:?}", rhs))
            },
            TokenType::Bang => Expression::Bool(!Self::is_truthy(&rhs)),
            _ => return Err(format!("Cannot apply '-' to {:?}", rhs))
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
}