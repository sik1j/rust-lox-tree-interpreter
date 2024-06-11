use crate::scanner::{Token, TokenType};

/// Grammar rules for lox:
/// expression -> equality ;
/// equality   -> comparison (("==" | "!=") comparison)* ;
/// comparison -> term (( ">" | ">=" | "<" | "<=" ) term)* ;
/// term       -> factor (( "-" | "+" ) factor )* ;
/// factor     -> unary (( "/" | "*" ) unary )* ;
/// unary      -> ( "!" | "-" ) unary | primary ;
/// primary    -> NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;

/// Types are not 1 to 1 with the grammar; deeply nested enums are impractical
pub enum ASTNode {
    Expression(Expression),
}

#[derive(Debug)]
pub enum Expression {
    Binary {
        left: Box<Expression>,
        operator: Token,
        right: Box<Expression>,
    },
    Unary {
        operator: Token,
        operand: Box<Expression>,
    },
    // Literals
    Number(f64),
    String(String),
    True, False, Nil,
}
use Expression as Expr;
use TokenType as Type;

pub(crate) struct Parser {
    tokens: Vec<Token>, // stored in reversed order so popping is more efficient
    position: usize,
}

impl Parser {
    pub fn new(mut tokens: Vec<Token>) -> Self {
        tokens.reverse();
        Self {
            tokens,
            position: 0,
        }
    }

    fn consume_token(&mut self) -> Token {
        let res = self.tokens.pop().expect("Expected non-empty array");
        self.position += 1;
        res
    }

    fn next_token(&self) -> &Token {
        self.tokens.last().unwrap()
    }

    fn is_next(&self, types: &[TokenType]) -> bool {
        if self.is_at_end() {
            return false;
        }
        let next_type = &self.next_token().token_type;

        types.contains(next_type)
    }

    fn is_at_end(&self) -> bool {
        self.next_token().token_type == Type::Eof
    }

    pub fn expression(&mut self) -> Expr {
        self.equality()
    }

    fn binary_parser(
        &mut self,
        parser: fn(&mut Self) -> Expression,
        operations: &[TokenType],
    ) -> Expr {
        let mut lhs = parser(self);

        while self.is_next(operations) {
            let op = self.consume_token();
            let rhs = parser(self);
            lhs = Expr::Binary {
                left: Box::from(lhs),
                operator: op,
                right: Box::from(rhs),
            };
        }

        lhs
    }

    fn equality(&mut self) -> Expr {
        self.binary_parser(Self::comparison, &[Type::BangEqual, Type::EqualEqual])
    }

    fn comparison(&mut self) -> Expr {
        self.binary_parser(
            Self::term,
            &[
                Type::Greater,
                Type::GreaterEqual,
                Type::Less,
                Type::LessEqual,
            ],
        )
    }

    fn term(&mut self) -> Expr {
        self.binary_parser(Self::factor, &[Type::Plus, Type::Minus])
    }

    fn factor(&mut self) -> Expr {
        self.binary_parser(Self::primary, &[Type::Star, Type::Slash])
    }

    pub fn primary(&mut self) -> Expr {
        let tok = self.consume_token();
        match tok.token_type {
            Type::Number(num) => Expr::Number(num),
            Type::String(str) => Expr::String(str),
            Type::True => Expr::True,
            Type::False => Expr::False,
            Type::Nil => Expr::Nil,
            Type::LeftParen => {
                let expr = self.expression();
                if self.consume_token().token_type != TokenType::RightParen {
                    panic!("Expected a closing ')'")
                }
                expr
            }
            _ => panic!("Unexpected token: {:?}", tok)
        }
    }
}
