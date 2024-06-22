// TODO: implement error handling that does not panic

use crate::scanner::{Token, TokenType};

/// Grammar rules for lox:
/// expression   -> assignment ;
/// assignment   -> IDENTIFIER '=' expression | logic_or ;
/// logic_or     -> logic_and ( "or" logic_and )* ;
/// logic_and    -> equality ( "and" equality )* ;
/// equality     -> comparison (("==" | "!=") comparison)* ;
/// comparison   -> term (( ">" | ">=" | "<" | "<=" ) term)* ;
/// term         -> factor (( "-" | "+" ) factor )* ;
/// factor       -> unary (( "/" | "*" ) unary )* ;
/// unary        -> ( "!" | "-" ) unary | call ;
/// call         -> primary ( "(" arguments? ")" )* ;
/// primary      -> NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" | IDENTIFIER ;
///
/// arguments    -> expression ( ',' expression )* ;
///
/// program      -> declaration* EOF ;
/// declaration  -> varDecl | statement ;
/// varDecl      -> "var" IDENTIFIER ( '=' expression )? ';'
///
/// statement    -> exprStmt | ifStmt | printStmt | whileStmt | forStmt | block ;
/// exprStmt     -> expression ';' ;
/// ifStmt       -> "if" '(' expression ')' statement ( "else" statement )? ;
/// printStmt    -> "print" expression ';' ;
/// whileStmt    -> "while" '(' expression ')' statement ;
/// forStmt      -> "for" '('
///                 ( (varDecl | expressionStmt ) | ';' )
///                 expression? ';'
///                 expression? ';' ')' statement ;
/// block        -> '{' declaration* '}'

/// Types are not 1 to 1 with the grammar; deeply nested enums are impractical
pub enum ASTNode {
    Statement(Statement),
}

#[derive(Debug, Clone)]
pub enum Statement {
    Print(Expression),
    Expression(Expression),
    VarDecl(Token, Option<Expression>),
    Block(Vec<Statement>),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    While(Expression, Box<Statement>),
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
    Logical(Box<Expression>, Token, Box<Expression>),
    Call {
        callee: Box<Expression>,
        arguments: Vec<Expression>,
        closing_paren: Token,
    },
    // Literals
    Number(f64),
    String(String),
    Bool(bool),
    Nil,
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

    fn expect_token(&mut self, expected: TokenType) -> Option<Token> {
        if self.is_next(&[expected]) {
            Some(self.consume_token())
        } else {
            None
        }
    }

    fn match_consume(&mut self, expected: TokenType) -> bool {
        if self.is_next(&[expected]) {
            self.consume_token();
            true
        } else {
            false
        }
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
        self.call()
    }

    fn call(&mut self) -> Expr {
        let mut expr = self.primary();

        loop {
            if self.match_consume(TokenType::LeftParen) {
                expr = self.finish_call(expr);
            } else {
                break;
            }
        }
        expr
    }
    fn finish_call(&mut self, callee: Expression) -> Expr {
        let mut arguments = vec![];

        if !self.is_next(&[TokenType::RightParen]) {
            loop {
                if arguments.len() >= 255 {
                    panic!("Cannot have more than 255 arguments");
                }
                arguments.push(self.expression());
                if !self.match_consume(TokenType::Comma) {
                    break;
                }
            }
        }

        let closing_paren = self
            .expect_token(TokenType::RightParen)
            .expect("Expected closing paren ')'");
        Expression::Call {
            arguments,
            callee: Box::from(callee),
            closing_paren,
        }
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
                self.expect_token(TokenType::RightParen)
                    .expect("Expected a closing ')'");
                Expr::Grouping(Box::from(expr))
            }
            Type::Identifier => Expr::Variable(tok),
            _ => panic!("Unexpected token: {:?}", tok),
        }
    }

    fn statement(&mut self) -> Statement {
        if self.match_consume(TokenType::Print) {
            self.print_statement()
        } else if self.match_consume(TokenType::LeftBrace) {
            self.block_statement()
        } else if self.match_consume(TokenType::If) {
            self.if_statement()
        } else if self.match_consume(TokenType::While) {
            self.while_statement()
        } else if self.match_consume(TokenType::For) {
            self.for_statement()
        } else {
            self.expression_statement()
        }
    }

    fn print_statement(&mut self) -> Statement {
        let expr = self.expression();
        self.expect_token(TokenType::Semicolon)
            .expect("Expected a ';'");
        Statement::Print(expr)
    }

    fn expression_statement(&mut self) -> Statement {
        let expr = self.expression();
        self.expect_token(TokenType::Semicolon)
            .expect("Expected a ';'");
        Statement::Expression(expr)
    }
    fn declaration(&mut self) -> Statement {
        if self.match_consume(TokenType::Var) {
            self.var_declaration()
        } else {
            self.statement()
        }
    }
    fn var_declaration(&mut self) -> Statement {
        let iden = self
            .expect_token(TokenType::Identifier)
            .expect("Expected an identifier");

        let mut initalizer = None;
        if self.match_consume(TokenType::Equal) {
            initalizer = Some(self.expression());
        }

        self.expect_token(TokenType::Semicolon)
            .expect("Expected a ';'");
        Statement::VarDecl(iden, initalizer)
    }
    fn assignment(&mut self) -> Expression {
        let expr = self.or();

        if self.is_next(&[TokenType::Equal]) {
            if let Expression::Variable(tok) = expr {
                self.consume_token();
                let val = self.assignment();
                return Expression::Assign(tok, Box::from(val));
            }
            panic!("Invalid assignment target");
        }

        expr
    }

    fn block_statement(&mut self) -> Statement {
        Statement::Block(self.parse_block())
    }

    fn parse_block(&mut self) -> Vec<Statement> {
        let mut statements = vec![];

        while !self.is_next(&[TokenType::RightBrace]) && !self.is_at_end() {
            statements.push(self.declaration());
        }
        self.expect_token(TokenType::RightBrace)
            .expect("Expected a closing '}'");
        statements
    }

    fn if_statement(&mut self) -> Statement {
        self.expect_token(TokenType::LeftParen)
            .expect("Expected a opening '('");
        let condition = self.expression();
        self.expect_token(TokenType::RightParen)
            .expect("Expected a closing ')'");

        let if_branch = self.statement();
        let mut else_branch = None;
        if self.match_consume(TokenType::Else) {
            else_branch = Some(Box::from(self.statement()));
        }
        Statement::If(condition, Box::from(if_branch), else_branch)
    }

    fn or(&mut self) -> Expr {
        let mut lhs = self.and();

        while self.is_next(&[TokenType::Or]) {
            let op = self.consume_token();
            let rhs = self.and();
            lhs = Expression::Logical(Box::from(lhs), op, Box::from(rhs));
        }

        lhs
    }

    fn and(&mut self) -> Expr {
        let mut lhs = self.equality();

        while self.is_next(&[TokenType::And]) {
            let op = self.consume_token();
            let rhs = self.equality();
            lhs = Expression::Logical(Box::from(lhs), op, Box::from(rhs));
        }

        lhs
    }

    fn while_statement(&mut self) -> Statement {
        self.expect_token(TokenType::LeftParen)
            .expect("Expected an opening '('");
        let condition = self.expression();
        self.expect_token(TokenType::RightParen)
            .expect("Expected a closing  ')'");

        let body = self.statement();

        Statement::While(condition, Box::from(body))
    }

    fn for_statement(&mut self) -> Statement {
        self.expect_token(TokenType::LeftParen)
            .expect("Expected an opening '('");

        let initalizer;
        if self.match_consume(TokenType::Semicolon) {
            initalizer = None;
        } else if self.match_consume(TokenType::Var) {
            initalizer = Some(self.var_declaration());
        } else {
            initalizer = Some(self.expression_statement());
        }

        let mut condition = None;
        if !self.is_next(&[TokenType::Semicolon]) {
            condition = Some(self.expression());
        }
        self.expect_token(TokenType::Semicolon)
            .expect("Expected a ';'");

        let mut side_effect = None;
        if !self.is_next(&[TokenType::RightParen]) {
            side_effect = Some(self.expression());
        }
        self.expect_token(TokenType::RightParen)
            .expect("Expected a ')'");

        // Statement::Block(vec![initalizer.unwrap(), self.statement()])

        let mut body = self.statement();
        if let Some(effect) = side_effect {
            body = Statement::Block(vec![body, Statement::Expression(effect)]);
        }

        let while_version = Statement::While(
            condition.unwrap_or_else(|| Expression::Bool(true)),
            Box::from(body),
        );

        Statement::Block(match initalizer {
            None => vec![while_version],
            Some(init) => vec![init, while_version],
        })
    }
}
