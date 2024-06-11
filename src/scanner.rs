use std::fmt::Formatter;
use std::str::FromStr;

#[derive(Debug, PartialEq)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier,
    String(String),
    Number(f64),

    // Keywords.
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Eof,
}

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: usize,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String, line: usize) -> Self {
        Token {
            token_type,
            lexeme,
            line,
        }
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

    pub had_error: bool,
}

impl Scanner {
    pub fn new(source: String) -> Self {
        Scanner {
            source,
            tokens: vec![],
            tok_start: 0,
            tok_curr: 0,
            tok_line: 1,
            had_error: false,
        }
    }

    pub fn scan_tokens(&mut self) -> Vec<Token> {
        while !self.is_at_end() {
            self.tok_start = self.tok_curr;
            self.scan_token();
        }

        self.tokens
            .push(Token::new(TokenType::Eof, "".to_string(), self.tok_line));
        std::mem::take(&mut self.tokens)
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
            ' ' => {}
            '\r' => {}
            '\t' => {}
            '\n' => self.tok_line += 1,
            // comments and slashes
            '/' => {
                if self.is_next('/') {
                    self.consume_comment()
                } else {
                    self.add_token(TokenType::Slash)
                }
            }
            // literal values
            '"' => self.consume_string(),
            '0'..='9' => self.consume_number(),
            // identifiers
            v if v.is_ascii_alphabetic() || v == '_' => self.consume_identifier(),
            // unknown chars
            _ => self.error(format!("Unexpected char: {:?}", c).as_str()),
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
        self.tokens
            .push(Token::new(token_type, text.to_string(), self.tok_line));
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

    fn peek(&self) -> char {
        if !self.is_at_end() {
            self.char_at(self.tok_curr)
        } else {
            '\0'
        }
    }

    fn peek_next(&self) -> char {
        if self.tok_curr + 1 < self.source.len() {
            self.char_at(self.tok_curr + 1)
        } else {
            '\0'
        }
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
        }
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
        let val = self.source[self.tok_start + 1..self.tok_curr - 1].to_string();
        self.add_token(TokenType::String(val));
    }
    fn consume_number(&mut self) {
        // pre decimal
        while self.peek().is_ascii_digit() {
            self.consume_char();
        }

        // trailing dots not allowed
        if self.is_next('.') && self.peek_next().is_ascii_digit() {
            self.consume_char();
        }

        // post decimal
        while self.peek().is_ascii_digit() {
            self.consume_char();
        }

        let num_str = &self.source[self.tok_start..self.tok_curr];
        let num = f64::from_str(num_str).expect("Expected a number");
        self.add_token(TokenType::Number(num));
    }
    fn consume_identifier(&mut self) {
        while self.is_next('_') || self.peek().is_ascii_alphanumeric() {
            self.consume_char();
        }

        let iden_or_keyword = &self.source[self.tok_start..self.tok_curr];
        let tok_type = Self::get_keyword(iden_or_keyword).unwrap_or(TokenType::Identifier);
        self.add_token(tok_type);
    }

    fn get_keyword(identifier: &str) -> Option<TokenType> {
        Some(match identifier {
            "and" => TokenType::And,
            "class" => TokenType::Class,
            "else" => TokenType::Else,
            "false" => TokenType::False,
            "for" => TokenType::For,
            "fun" => TokenType::Fun,
            "if" => TokenType::If,
            "nil" => TokenType::Nil,
            "or" => TokenType::Or,
            "print" => TokenType::Print,
            "return" => TokenType::Return,
            "super" => TokenType::Super,
            "this" => TokenType::This,
            "true" => TokenType::True,
            "var" => TokenType::Var,
            "while" => TokenType::While,
            _ => return None,
        })
    }
}
