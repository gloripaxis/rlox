use std::{collections::HashMap, error::Error};

use types::{LexerError, Token, TokenType};

use crate::LoxError;

pub mod types;

pub struct Lexer<'a> {
    start: usize,
    current: usize,
    line: usize,

    source: &'a str,
    srclen: usize,
    chars: Vec<char>,

    keywords: HashMap<&'static str, TokenType>,
    errors: Vec<LexerError>
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
            keywords: kwds,
            errors: Vec::new()
        }
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

    fn consolidate_errors(&mut self) -> Box<dyn Error> {
        let mut total_error = String::new();
        if self.errors.len() > 1 {
            total_error.push_str("Multiple errors occured in Lexer:\n");
        }
        for err in self.errors.iter() {
            total_error.push_str(&format!("{err}"));
            total_error.push('\n');
        }
        Box::new(LoxError::new(total_error))
    }

    pub fn scan(&mut self) -> Result<Vec<Token>, Box<dyn Error>> {
        let mut tokens: Vec<Token> = Vec::new();
        
        while !self.is_end() {
            self.start = self.current;
            let c: char = self.advance();

            match c {
                // Unambiguous single-symbol tokens
                '(' => tokens.push(Token::new(TokenType::LeftParen, "(", self.line)),
                ')' => tokens.push(Token::new(TokenType::RightParen, ")", self.line)),
                '{' => tokens.push(Token::new(TokenType::LeftBrace, "{", self.line)),
                '}' => tokens.push(Token::new(TokenType::RightBrace, "}", self.line)),
                ',' => tokens.push(Token::new(TokenType::Comma, ",", self.line)),
                '.' => tokens.push(Token::new(TokenType::Dot, ".", self.line)),
                '-' => tokens.push(Token::new(TokenType::Minus, "-", self.line)),
                '+' => tokens.push(Token::new(TokenType::Plus, "+", self.line)),
                ';' => tokens.push(Token::new(TokenType::Semicolon, ";", self.line)),
                '*' => tokens.push(Token::new(TokenType::Star, "*", self.line)),
                // Ambiguous single-, double-, or multi-symbol tokens
                '!' => {
                    if self.advance_maybe('=') {
                        tokens.push(Token::new(TokenType::Neq, "!=", self.line));
                    } else {
                        tokens.push(Token::new(TokenType::Bang, "!", self.line));
                    }
                },
                '=' => {
                    if self.advance_maybe('=') {
                        tokens.push(Token::new(TokenType::Eq, "==", self.line));
                    } else {
                        tokens.push(Token::new(TokenType::Assign, "=", self.line));
                    }
                },
                '<' => {
                    if self.advance_maybe('=') {
                        tokens.push(Token::new(TokenType::Leq, "<=", self.line));
                    } else {
                        tokens.push(Token::new(TokenType::Lt, "<", self.line));
                    }
                },
                '>' => {
                    if self.advance_maybe('=') {
                        tokens.push(Token::new(TokenType::Geq, ">=", self.line));
                    } else {
                        tokens.push(Token::new(TokenType::Gt, ">", self.line));
                    }
                },
                '/' => {
                    if self.advance_maybe('/') {
                        self.advance_until('\n');
                    } else {
                        tokens.push(Token::new(TokenType::Slash, "/", self.line));
                    }
                },
                // Whitespace
                ' ' | '\r' | '\t' => {},
                '\n' => { self.line += 1; },
                // Strings
                '"' => {
                    self.advance_until('"');
                    println!("{}/{}", self.current, self.srclen);
                    if self.is_end() {
                        // This is a non-recoverable error => early return Err
                        self.errors.push(LexerError::new(String::from("Unterminated string"), self.line));
                        let lox_error = self.consolidate_errors();
                        return Err(lox_error);
                    }
                    self.advance();
                    let value = &self.source[self.start+1..self.current-1];
                    tokens.push(Token::new(TokenType::String, value, self.line));
                },
                // Numbers
                '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
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
                    tokens.push(Token::new(TokenType::Number, value, self.line));
                },
                _ => {
                    if !c.is_ascii_alphabetic() {
                        self.errors.push(LexerError::new(String::from(format!("Unexpected character: {c}")), self.line));
                        continue;
                    }
                    // Identifiers & Keywords
                    while self.peek().is_ascii_alphanumeric() {
                        self.advance();
                    }
                    
                    let value = &self.source[self.start..self.current];
                    let ttype = self.keywords.get(value).copied().unwrap_or(TokenType::Identifier);
                    tokens.push(Token::new(ttype, value, self.line));
                }
            }
        }
        
        if !self.errors.is_empty() {
            let lox_error = self.consolidate_errors();
            return Err(lox_error);
        }

        let eof_token = Token::new(TokenType::EOF, "\0", self.line);
        tokens.push(eof_token);
        Ok(tokens)
    }
}
