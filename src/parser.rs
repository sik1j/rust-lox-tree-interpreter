// TODO: implement error handling that does not panic

use crate::scanner::{Token, TokenType};

/// Grammar rules for lox:
/// expression   -> assignment ;
/// assignment   -> IDENTIFIER '=' expression | equality ;
/// equality     -> comparison (("==" | "!=") comparison)* ;
/// comparison   -> term (( ">" | ">=" | "<" | "<=" ) term)* ;
/// term         -> factor (( "-" | "+" ) factor )* ;
/// factor       -> unary (( "/" | "*" ) unary )* ;
/// unary        -> ( "!" | "-" ) unary | primary ;
/// primary      -> NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" | IDENTIFIER ;
///
/// program      -> declaration* EOF ;
/// declaration  -> varDecl | statement ;
/// varDecl      -> "var" IDENTIFIER ( '=' expression )? ';'
/// statement    -> exprStmt | printStmt ;
/// exprStmt     -> expression ';' ;
/// printStmt    -> "print" expression ';' ;

/// Types are not 1 to 1 with the grammar; deeply nested enums are impractical
pub enum ASTNode {
    Statement(Statement),
}

#[derive(Debug)]
pub enum Statement {
    Print(Expression),
    Expression(Expression),
    VarDecl(Token, Option<Expression>)
}

#[derive(Debug, Clone)]
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
    Grouping(Box<Expression>),
    Assign(Token, Box<Expression>),
    Variable(Token),
    // Literals
    Number(f64),
    String(String),
    Bool(bool),
    Nil,
}
use Expression as Expr;
use TokenType as Type;
use crate::parser::Expr::Assign;

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

    pub fn parse(&mut self) -> Vec<Statement> {
        let mut statements: Vec<Statement> = vec![];
        while !self.is_at_end() {
            statements.push(self.declaration());
        }
        statements
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

    fn expression(&mut self) -> Expr {
        self.assignment()
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
        self.binary_parser(Self::unary, &[Type::Star, Type::Slash])
    }

    fn unary(&mut self) -> Expr {
        if self.is_next(&[TokenType::Bang, TokenType::Minus]) {
            let op = self.consume_token();
            let rhs = self.unary();

            return Expr::Unary {
                operator: op,
                operand: Box::from(rhs),
            };
        }
        self.primary()
    }

    fn primary(&mut self) -> Expr {
        let tok = self.consume_token();
        match tok.token_type {
            Type::Number(num) => Expr::Number(num),
            Type::String(str) => Expr::String(str),
            Type::True => Expr::Bool(true),
            Type::False => Expr::Bool(false),
            Type::Nil => Expr::Nil,
            Type::LeftParen => {
                let expr = self.expression();
                self.expect_token(TokenType::RightParen).expect("Expected a closing ')'");
                Expr::Grouping(Box::from(expr))
            }
            Type::Identifier => Expr::Variable(tok),
            _ => panic!("Unexpected token: {:?}", tok),
        }
    }

    fn expect_token(&mut self, expected: TokenType) -> Option<Token> {
        if self.is_next(&[expected]) {
            Some(self.consume_token())
        } else {
            None
        }
    }

    fn statement(&mut self) -> Statement {
        if self.is_next(&[TokenType::Print]) {
            self.consume_token();
            self.print_statement()
        } else {
            self.expression_statement()
        }
    }

    fn print_statement(&mut self) -> Statement {
        let expr = self.expression();
        self.expect_token(TokenType::Semicolon).expect("Expected a ';'");
        Statement::Print(expr)
    }


    fn expression_statement(&mut self) -> Statement {
        let expr = self.expression();
        self.expect_token(TokenType::Semicolon).expect("Expected a ';'");
        Statement::Expression(expr)
    }
    fn declaration(&mut self) -> Statement {
        if self.is_next(&[TokenType::Var]) {
            self.consume_token();
            self.var_declaration()
        } else {
            self.statement()
        }
    }
    fn var_declaration(&mut self) -> Statement {
        let iden = self.expect_token(TokenType::Identifier).expect("Expected an identifier");

        let mut initalizer = None;
        if self.is_next(&[TokenType::Equal]) {
            self.consume_token();
            initalizer = Some(self.expression());
        }

        self.expect_token(TokenType::Semicolon).expect("Expected a ';'");
        Statement::VarDecl(iden, initalizer)
    }
    fn assignment(&mut self) -> Expression {
        let expr = self.equality();

        if self.is_next(&[TokenType::Equal]) {
            if let Expression::Variable(tok) = expr {
                self.consume_token();
                let val = self.assignment();
                return Assign(tok, Box::from(val));
            }
            panic!("Invalid assignment target");
        }

        expr
    }
}
