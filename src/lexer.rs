use std::rc::Rc;

use crate::errors::{ErrorInfo, LoxError};
use token::{Literal, Token, TokenType};

pub mod token;

pub struct Lexer<'a> {
    start: usize,
    current: usize,
    line: usize,
    column: usize,

    source: &'a str,
    srclen: usize,
    chars: Vec<char>,
    tokens: Vec<Token>,

    errors: Vec<ErrorInfo>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        let chars: Vec<char> = source.chars().collect();
        let srclen = chars.len();

        Self {
            start: 0,
            current: 0,
            line: 1,
            column: 1,
            source,
            chars,
            srclen,
            tokens: Vec::new(),
            errors: Vec::new(),
        }
    }

    pub fn scan(mut self) -> Result<Vec<Token>, LoxError> {
        while !self.is_end() {
            self.start = self.current;
            self.scan_token();
        }

        if !self.errors.is_empty() {
            return Err(LoxError::Lexer(self.errors));
        }

        let eof_token = Token::new(
            TokenType::EndOfFile,
            Rc::from("\0"),
            Literal::Nil,
            self.line,
            self.column,
        );
        self.tokens.push(eof_token);
        Ok(self.tokens)
    }

    fn scan_token(&mut self) {
        let c: char = self.advance();
        match c {
            // Unambiguous single-symbol tokens
            '(' | ')' | '{' | '}' | ',' | '.' | '-' | '+' | ';' | '*' => self.add_token(TokenType::from_char(c), None),

            // Ambiguous single-, double-, or multi-symbol tokens
            '!' | '=' | '<' | '>' => self.scan_either('=', TokenType::from_pre_eq_char(c), TokenType::from_char(c)),

            // Division or Comments
            '/' => match self.advance_maybe('/') {
                true => self.advance_until('\n'),
                false => self.add_token(TokenType::Slash, None),
            },
            // Regular whitespace
            ' ' | '\r' | '\t' => {}

            // Newlines
            '\n' => {
                self.line += 1;
                self.column = 1;
            }

            // Strings
            '"' => self.scan_string(),

            // Numbers
            '0'..='9' => self.scan_number(),

            // Identifiers & Keywords
            'A'..='Z' | 'a'..='z' => self.scan_identifier_or_keyword(),

            // Unexpected characters
            _ => self.store_error(format!("Encountered illegal character '{c}'"), None),
        }
    }

    fn scan_either(&mut self, expected: char, double: TokenType, single: TokenType) {
        let ttype = if self.advance_maybe(expected) { double } else { single };
        self.add_token(ttype, None);
    }

    fn scan_string(&mut self) {
        self.advance_until('"');
        if self.is_end() {
            // This is a non-recoverable error => early return
            let start_position = self.find_string_start_position(&self.source[self.start..self.current]);
            self.store_error(String::from("String is never terminated"), Some(start_position));
            return;
        }
        self.advance();
        let value = &self.source[self.start + 1..self.current - 1];
        self.add_token(TokenType::String, Some(String::from(value)));
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

        self.add_token(TokenType::Number, None);
    }

    fn scan_identifier_or_keyword(&mut self) {
        while self.peek().is_ascii_alphanumeric() {
            self.advance();
        }

        let value = &self.source[self.start..self.current];
        let ttype = TokenType::get_keyword_type(value).unwrap_or(TokenType::Identifier);
        self.add_token(ttype, Some(String::from(value)));
    }

    fn find_string_start_position(&self, value: &str) -> (usize, usize) {
        let count_newlines = value.chars().filter(|c| *c == '\n').count();
        let true_line = self.line - count_newlines;

        let mut line_start: usize = self.start;
        loop {
            if line_start == 0 {
                break;
            }
            if self.chars[line_start] == '\n' {
                line_start += 1;
                break;
            }
            line_start -= 1;
        }
        let true_start = self.start - line_start + 1;

        (true_line, true_start)
    }

    fn add_token(&mut self, ttype: TokenType, value: Option<String>) {
        let true_value = value.unwrap_or(String::from(&self.source[self.start..self.current]));
        let (true_line, true_start) = match ttype {
            TokenType::String => self.find_string_start_position(&true_value),
            _ => (self.line, self.column - 1),
        };

        let rc_value: Rc<str> = Rc::from(true_value);

        let literal = match ttype {
            TokenType::String => Literal::String(Rc::clone(&rc_value)),
            TokenType::Number => Literal::Number(Rc::clone(&rc_value).parse().unwrap()),
            TokenType::True => Literal::Boolean(true),
            TokenType::False => Literal::Boolean(false),
            _ => Literal::Nil,
        };

        self.tokens
            .push(Token::new(ttype, rc_value, literal, true_line, true_start))
    }

    fn is_end(&self) -> bool {
        self.current >= self.srclen
    }

    fn advance(&mut self) -> char {
        let c: char = self.chars[self.current];
        self.current += 1;
        self.column += 1;
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
        self.column += 1;
        true
    }

    fn advance_until(&mut self, expected: char) {
        while self.peek() != expected && !self.is_end() {
            if self.peek() == '\n' {
                self.line += 1;
                self.column = 1;
            }
            self.advance();
        }
    }

    fn peek(&self) -> char {
        if self.is_end() {
            return '\0';
        }
        self.chars[self.current]
    }

    fn peek_next(&self) -> char {
        if self.current + 1 > self.srclen {
            return '\0';
        }
        self.chars[self.current + 1]
    }

    fn store_error(&mut self, message: String, start_pos: Option<(usize, usize)>) {
        let einfo = match start_pos {
            Some((l, c)) => ErrorInfo::with_start((l, c), (self.line, self.column - 1), message.to_string()),
            _ => ErrorInfo::new((self.line, self.column - 1), message.to_string()),
        };
        self.errors.push(einfo);
    }
}
