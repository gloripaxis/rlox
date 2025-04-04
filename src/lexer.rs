use std::rc::Rc;

use crate::errors::{ErrorInfo, LoxError};
use token::{Literal, Token, TokenType};

pub mod token;

pub struct Lexer<'a> {
    start: usize,
    current: usize,
    line: usize,
    column: usize,

    str_start_pos: Option<(usize, usize)>,

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
            str_start_pos: None,
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
                true => self.advance_line(),
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
        self.str_start_pos = Some((self.line, self.column));
        self.advance_string();
        if self.is_end() {
            // This is a non-recoverable error => early return
            self.store_error(String::from("String is never terminated"), self.str_start_pos);
            return;
        }
        self.advance(); // consume closing "
        self.add_token(TokenType::String, Some(self.unescape_string()));
        self.str_start_pos = None;
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
                        self.line += 1;
                        self.column = 0; // 0 because self.advance() will add 1
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

    fn store_error(&mut self, message: String, start_pos: Option<(usize, usize)>) {
        let einfo = match start_pos {
            Some((l, c)) => ErrorInfo::with_start((l, c), (self.line, self.column - 1), message.to_string()),
            _ => ErrorInfo::new((self.line, self.column - 1), message.to_string()),
        };
        self.errors.push(einfo);
    }
}
