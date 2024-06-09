use std::fmt::{Formatter};

#[derive(Debug)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen, RightParen, LeftBrace, RightBrace,
    Comma, Dot, Minus, Plus, Semicolon, Slash, Star,

    // One or two character tokens.
    Bang, BangEqual,
    Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,

    // Literals.
    Identifier, String(String), Number,

    // Keywords.
    And, Class, Else, False, Fun, For, If, Nil, Or,
    Print, Return, Super, This, True, Var, While,

    Eof
}

pub struct Token {
    token_type: TokenType,
    lexeme: String,
    line: usize,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String, line: usize) -> Self {
        Token {token_type, lexeme, line}
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} {:?} {:?}", self.token_type, self.lexeme, self.line)
    }
}

pub struct Scanner {
    source: String, // source code
    tokens: Vec<Token>,

    tok_start: usize,
    tok_curr: usize,
    tok_line: usize,

    had_error: bool,
}

impl Scanner {
    pub fn new(source: String) -> Self {
        Scanner {source, tokens: vec![], tok_start: 0, tok_curr: 0, tok_line: 1, had_error: false}
    }

    pub fn scan_tokens(&mut self) -> &Vec<Token> {
        while !self.is_at_end() {
            self.tok_start = self.tok_curr;
            self.scan_token();
        }

        self.tokens.push(Token::new(TokenType::Eof, "".to_string(), self.tok_line));
        &self.tokens
    }

    fn is_at_end(&self) -> bool {
        self.tok_curr >= self.source.len()
    }
    fn scan_token(&mut self) {
        let c = self.consume_char();
        match c {
            // 1 char tokens
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            '{' => self.add_token(TokenType::LeftBrace),
            '}' => self.add_token(TokenType::RightBrace),
            ',' => self.add_token(TokenType::Comma),
            '.' => self.add_token(TokenType::Dot),
            '-' => self.add_token(TokenType::Minus),
            '+' => self.add_token(TokenType::Plus),
            ';' => self.add_token(TokenType::Semicolon),
            '*' => self.add_token(TokenType::Star),
            // 1-2 char tokens
            '!' => self.check_next_and_add('=', TokenType::Bang, TokenType::BangEqual),
            '=' => self.check_next_and_add('=', TokenType::Equal, TokenType::EqualEqual),
            '<' => self.check_next_and_add('=', TokenType::Less, TokenType::LessEqual),
            '>' => self.check_next_and_add('=', TokenType::Greater, TokenType::GreaterEqual),
            // white space
            ' ' => {},
            '\r' => {},
            '\t' => {},
            '\n' => {self.tok_line += 1},
            // comments and slashes
            '/' => if self.is_next('/') {self.consume_comment()}
                else {self.add_token(TokenType::Slash)},
            // literal values
            '"' => self.consume_string(),
            // unknown chars
            _=> self.error(format!("Unexpected char: {:?}", c).as_str()),
        }
    }
    fn char_at(&self, ind: usize) -> char {
        self.source.as_bytes()[ind] as char
    }
    fn consume_char(&mut self) -> char {
        let next = self.char_at(self.tok_curr);
        self.tok_curr += 1;
        next
    }
    fn add_token(&mut self, token_type: TokenType) {
        let text = &self.source[self.tok_start..self.tok_curr];
        self.tokens.push(Token::new(token_type, text.to_string(), self.tok_line));
    }
    fn error(&mut self, message: &str) {
        self.report("", message);
    }

    fn report(&mut self, wher: &str, message: &str) {
        eprintln!("[line {}] Error{}: {}", self.tok_line, wher, message);
        self.had_error = true;
    }
    fn is_next(&self, c: char) -> bool {
        !self.is_at_end() && self.char_at(self.tok_curr) == c
    }
    /// If the next char matches `check`, then adds `two_char` token, else `one_char` token.
    /// also consumes char in source if necessary
    /// # Arguments
    ///
    /// * `check`: The second char of the two char token to check against
    /// * `one_char`:
    /// * `two_char`:
    ///
    /// returns: ()
    ///
    /// # Examples
    ///
    /// ```
    ///
    /// ```
    fn check_next_and_add(&mut self, check: char, one_char: TokenType, two_char: TokenType) {
        if self.is_next(check) {
            self.tok_curr += 1;
            self.add_token(two_char);
        } else {
            self.add_token(one_char);
        }
    }

    fn consume_comment(&mut self) {
        while !self.is_at_end() && !self.is_next('\n') {
            self.consume_char();
        };
    }
    fn consume_string(&mut self) {
        while !self.is_at_end() && !self.is_next('"') {
            if self.is_next('\n') {
                self.tok_line += 1;
            }
            self.consume_char();
        }

        if self.is_at_end() {
            self.error("Unterminated string");
        }

        self.consume_char(); // consume closing "

        // trim surrounding quotes
        let val = self.source[self.tok_start+1..self.tok_curr-1].to_string();
        self.add_token(TokenType::String(val));
    }
}