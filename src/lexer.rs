use std::{collections::HashMap, error::Error};
use token::{Token, TokenType};
use crate::errors::{ErrorMessage, ErrorType, RloxError};

pub mod token;

pub struct Lexer<'a> {
    start: usize,
    current: usize,
    line: usize,

    source: &'a str,
    srclen: usize,
    chars: Vec<char>,
    tokens: Vec<Token<'a>>,

    keywords: HashMap<&'static str, TokenType>,
    errors: Vec<ErrorMessage>
}

impl <'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        let chars: Vec<char> = source.chars().collect();
        let srclen = chars.len();
        let kwds = HashMap::from([
            ("and", TokenType::And),
            ("class", TokenType::Class),
            ("else", TokenType::Else),
            ("false", TokenType::False),
            ("for", TokenType::For),
            ("fun", TokenType::Fun),
            ("if", TokenType::If),
            ("nil", TokenType::Nil),
            ("or", TokenType::Or),
            ("print", TokenType::Print),
            ("return", TokenType::Return),
            ("super", TokenType::Super),
            ("this", TokenType::This),
            ("true", TokenType::True),
            ("var", TokenType::Var),
            ("while", TokenType::While)
        ]);

        Self {
            start: 0,
            current: 0,
            line: 1, 
            source: source,
            chars: chars,
            srclen: srclen, 
            tokens: Vec::new(),
            keywords: kwds,
            errors: Vec::new()
        }
    }

    fn add_token(&mut self, ttype: TokenType) {
        self.tokens.push(Token::new(ttype, &self.source[self.start..self.current], self.line))
    }

    fn is_end(&self) -> bool {
        self.current >= self.srclen
    }

    fn peek(&self) -> char {
        if self.is_end() {
            return '\0';
        }
        self.chars[self.current]
    }

    fn peek_next(&self) -> char {
        if self.current + 1 > self.srclen {
            return '\0'
        }
        self.chars[self.current+1]
    }

    fn advance(&mut self) -> char {
        let c: char = self.chars[self.current];
        self.current += 1;
        c
    }

    fn advance_maybe(&mut self, expected: char) -> bool {
        if self.is_end() {
            return false;
        }
        if self.chars[self.current] != expected {
            return false;
        }
        self.current += 1;
        true
    }

    fn advance_until(&mut self, expected: char) {
        while self.peek() != expected && !self.is_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }
    }

    fn store_error(&mut self, message: String) {
        self.errors.push(ErrorMessage::new(ErrorType::LexerError, message, self.line))
    }

    fn build_error(&mut self) -> Box<dyn Error> {
        Box::new(RloxError::new(self.errors.clone()))
    }

    fn choose(&mut self, expected: char, double: TokenType, single: TokenType) {
        let ttype = if self.advance_maybe(expected) { double } else { single };
        self.add_token(ttype);
    }

    fn scan_string(&mut self) -> Result<(), Box<dyn Error>> {
        self.advance_until('"');
        if self.is_end() {
            // This is a non-recoverable error => early return Err
            self.store_error(String::from("Unterminated string"));
            return Err(self.build_error());
        }
        self.advance();
        let value = &self.source[self.start+1..self.current-1];
        self.tokens.push(Token::new(TokenType::String, value, self.line));
        Ok(())
    }

    fn scan_number(&mut self) {
        while self.peek().is_ascii_digit() {
            self.advance();
        }
        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            self.advance();
        }
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        let value = &self.source[self.start..self.current];
        self.tokens.push(Token::new(TokenType::Number, value, self.line));
    }

    fn scan_identifier(&mut self) {
        while self.peek().is_ascii_alphanumeric() {
            self.advance();
        }
        
        let value = &self.source[self.start..self.current];
        let ttype = self.keywords.get(value).copied().unwrap_or(TokenType::Identifier);
        self.tokens.push(Token::new(ttype, value, self.line));
    }

    fn scan_token(&mut self) -> Result<(), Box<dyn Error>> {
        let c: char = self.advance();
        match c {
            // Unambiguous single-symbol tokens
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

            // Ambiguous single-, double-, or multi-symbol tokens
            '!' => self.choose('=', TokenType::Neq, TokenType::Bang),
            '=' => self.choose('=', TokenType::Eq, TokenType::Assign),
            '<' => self.choose('=', TokenType::Leq, TokenType::Lt),
            '>' => self.choose('=', TokenType::Geq, TokenType::Gt),

            // Division or Comments
            '/' => {
                if self.advance_maybe('/') {
                    self.advance_until('\n');
                } else {
                    self.add_token(TokenType::Slash);
                }
            },
            // Regular whitespace
            ' ' | '\r' | '\t' => {},

            // Newlines
            '\n' => self.line += 1,
            
            // Strings
            '"' => self.scan_string()?, // ? required because scan_string can throw an unrecoverable error
            
            // Numbers
            '0'..='9' => self.scan_number(),
            
            // Identifiers & Keywords
            'A'..='Z' | 'a'..='z' => self.scan_identifier(),
            
            // Unexpected characters
            _ => self.store_error(format!("Unexpected character: '{c}'"))
        }
        Ok(())
    }

    pub fn scan(mut self) -> Result<Vec<Token<'a>>, Box<dyn Error>> {
        while !self.is_end() {
            self.start = self.current;
            self.scan_token()?;
        }
        
        if !self.errors.is_empty() {
            return Err(self.build_error());
        }

        let eof_token = Token::new(TokenType::EOF, "\0", self.line);
        self.tokens.push(eof_token);
        Ok(self.tokens)
    }

}
