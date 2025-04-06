use std::rc::Rc;

use crate::errors::{ErrorInfo, LoxError};
use crate::types::position::Pos;
use crate::types::{literal::Lit, token::Token, token::TokenType};

pub struct Lexer<'a> {
    start: usize,
    current: usize,

    start_pos: Pos,
    cur_pos: Pos,

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
            start_pos: Pos::new(1, 1),
            cur_pos: Pos::new(1, 1),
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
            self.start_pos = self.cur_pos;
            self.scan_token();
        }

        if !self.errors.is_empty() {
            return Err(LoxError::Lexer(self.errors));
        }

        let eof_token = Token::new(TokenType::EndOfFile, Lit::Nil, self.cur_pos);
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
                true => self.advance_line(),
                false => self.add_token(TokenType::Slash, None),
            },
            // Regular whitespace
            ' ' | '\r' | '\t' => {}

            // Newlines
            '\n' => self.cur_pos.newline(None),

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
        self.advance_string();
        if self.is_end() {
            // This is a non-recoverable error => early return
            self.store_error(String::from("String is never terminated"), Some(self.start_pos));
            return;
        }
        self.advance(); // consume closing "
        self.add_token(TokenType::String, Some(self.unescape_string()));
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

    fn unescape_string(&self) -> String {
        let mut s = String::new();
        let mut is_escaped = false;
        for i in self.start + 1..self.current - 1 {
            if !is_escaped {
                match self.chars[i] {
                    '\\' => is_escaped = true,
                    x => s.push(x),
                };
            } else {
                s.push(self.chars[i]);
                is_escaped = false;
            }
        }
        s
    }

    fn add_token(&mut self, ttype: TokenType, value: Option<String>) {
        let true_value = value.unwrap_or(String::from(&self.source[self.start..self.current]));
        let rc_value: Rc<str> = Rc::from(true_value);

        let literal = match ttype {
            TokenType::String => Lit::Str(Rc::clone(&rc_value)),
            TokenType::Number => Lit::Num(rc_value.parse().unwrap()),
            TokenType::True => Lit::Bool(true),
            TokenType::False => Lit::Bool(false),
            TokenType::Identifier => Lit::Id(Rc::clone(&rc_value)),
            _ => Lit::Nil,
        };

        self.tokens.push(Token::new(ttype, literal, self.start_pos))
    }

    fn is_end(&self) -> bool {
        self.current >= self.srclen
    }

    fn advance(&mut self) -> char {
        let c: char = self.chars[self.current];
        self.current += 1;
        self.cur_pos.next();
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
        self.cur_pos.next();
        true
    }

    fn advance_line(&mut self) {
        while self.peek() != '\n' && !self.is_end() {
            self.advance();
        }
    }

    fn advance_string(&mut self) {
        let mut is_escaped = false;

        while !self.is_end() {
            // If last character wasn't '\\'
            if !is_escaped {
                // Then the three special cases are '\\', '\n' and '"'
                match self.peek() {
                    // if \, the next character is escaped
                    '\\' => is_escaped = true,
                    // if \n, update line and column counters
                    '\n' => {
                        self.cur_pos.newline(Some(0));
                    }
                    // if ", finish loop
                    '"' => break,
                    _ => {}
                }
            // If last character was '\'
            } else {
                // Then the next character can be either '\\' or '"', otherwise it's an error
                match self.peek() {
                    // If legal escape sequence, escaping is finished
                    '\\' | '"' => {}
                    // Otherwise it's an error, but lexing can continue - at worst we'll reach EOF, at best there will be another " somewhere
                    _ => {
                        self.store_error(format!("Invalid escape sequence: \\{}", self.peek()), None);
                    }
                }
                is_escaped = false;
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

    fn store_error(&mut self, message: String, pos: Option<Pos>) {
        let einfo = match pos {
            Some(p) => ErrorInfo::with_start(p, self.cur_pos.left(), message.to_string()),
            _ => ErrorInfo::new(self.cur_pos.left(), message.to_string()),
        };
        self.errors.push(einfo);
    }
}
