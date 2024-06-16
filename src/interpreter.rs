use crate::parser::Expression;
use Expression as Expr;
use crate::scanner::{Token, TokenType};

#[derive(Debug)]
pub struct Interpreter {}

impl Interpreter {
    pub fn evaluate(expr: Expr) -> Expr {
        match expr {
            Expr::Binary { left, operator, right } => Self::eval_binary(*left, operator, *right),
            Expr::Unary { operator, operand } => Self::eval_unary(operator, *operand),
            Expr::Grouping(expr) => Self::evaluate(*expr),
            _ => expr
        }
    }

    fn eval_binary(lhs: Expression, op: Token, rhs: Expression) -> Expression {
        let lhs = Self::evaluate(lhs);
        let rhs = Self::evaluate(rhs);

        match &op.token_type {
            TokenType::EqualEqual => Expr::Bool(Self::is_equal(&lhs, &rhs)),
            TokenType::BangEqual => Expr::Bool(!Self::is_equal(&lhs, &rhs)),
            _ =>
            match (&lhs, &rhs) {
                (Expr::Number(lhs), Expr::Number(rhs)) => match &op.token_type {
                    TokenType::Plus => Expr::Number(lhs + rhs),
                    TokenType::Minus => Expr::Number(lhs - rhs),
                    TokenType::Star => Expr::Number(lhs * rhs),
                    TokenType::Slash => Expr::Number(lhs / rhs),
                    TokenType::Greater => Expr::Bool(lhs > rhs),
                    TokenType::GreaterEqual => Expr::Bool(lhs >= rhs),
                    TokenType::Less => Expr::Bool(lhs < rhs),
                    TokenType::LessEqual => Expr::Bool(lhs <= rhs),
                    _ => panic!("Cannot perform '{} {} {}'", lhs, op.lexeme, rhs)
                },
                (Expr::String(lhs), Expr::String(rhs)) => Expr::String(match &op.token_type {
                    TokenType::Plus => format!("{}{}", lhs, rhs),
                    _ => panic!("Cannot perform '{} {} {}'", lhs, op.lexeme, rhs),
                }),
                _ => panic!("Cannot perform '{:?} {} {:?}'", lhs, op.lexeme, rhs),
            }
        }

    }

    fn eval_unary(operator: Token, operand: Expr) -> Expr {
        let rhs = Self::evaluate(operand);

        match operator.token_type {
            TokenType::Minus => match &rhs {
                Expr::Number(n) => Expression::Number(-n),
                _ => panic!("Cannot apply '-' to {:?}", rhs)
            },
            TokenType::Bang => Expr::Bool(!Self::is_truthy(&rhs)),
            _ => panic!("Cannot apply '-' to {:?}", rhs)
        }
    }

    fn is_truthy(expr: &Expr) -> bool {
        match expr {
            &Expr::Bool(b) => b,
            Expr::Nil => false,
            _ => true,
        }
    }
    fn is_equal(e1: &Expression, e2: &Expression) -> bool {
        match (e1, e2) {
            (Expr::Number(n), Expr::Number(m)) => n == m,
            (Expr::String(n), Expr::String(m)) => n == m,
            (Expr::Bool(n), Expr::Bool(m)) => n == m,
            (Expr::Nil, Expr::Nil) => true,
            _ => false,
        }
    }
}