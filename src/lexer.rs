use std::error::Error;

use types::{LexerError, Token, TokenType};

use crate::LoxError;

pub mod types;

fn is_end(current: usize, srclen: usize) -> bool {
    current >= srclen
}

fn advance_maybe(current: &mut usize, srclen: usize, chars: &[char], expected: char) -> bool {
    if is_end(*current, srclen) {
        return false;
    }
    if chars[*current] != expected {
        return false;
    }

    *current = *current + 1;
    true
}

fn peek(current: usize, srclen: usize, chars: &[char]) -> char {
    if is_end(current, srclen) {
        return '\0';
    }
    chars[current]
}

fn advance(chars: &[char], current: &mut usize) -> char {
    let c: char = chars[*current];
    *current = *current + 1;
    c
}

fn consolidate_errors(errors: Vec<LexerError>) -> Box<dyn Error> {
    let mut total_error = String::new();
    if errors.len() > 1 {
        total_error.push_str("Multiple errors occured in Lexer:\n");
    }
    for err in errors.iter() {
        total_error.push_str(&format!("{err}"));
        total_error.push('\n');
    }
    Box::new(LoxError::new(total_error))
}

pub fn scan(source: &str) -> Result<Vec<Token>, Box<dyn Error>> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut start: usize;
    let mut current: usize = 0;
    let mut line: usize = 1;

    let mut errors: Vec<LexerError> = Vec::new();

    let chars: Vec<char> = source.chars().collect();
    let srclen = chars.len();
    while !is_end(current, srclen) {
        start = current;

        // advance
        let c = advance(&chars, &mut current);
        
        // scanToken
        match c {
            '(' => tokens.push(Token::new(TokenType::LeftParen, "(", line)),
            ')' => tokens.push(Token::new(TokenType::RightParen, ")", line)),
            '{' => tokens.push(Token::new(TokenType::LeftBrace, "{", line)),
            '}' => tokens.push(Token::new(TokenType::RightBrace, "}", line)),
            ',' => tokens.push(Token::new(TokenType::Comma, ",", line)),
            '.' => tokens.push(Token::new(TokenType::Dot, ".", line)),
            '-' => tokens.push(Token::new(TokenType::Minus, "-", line)),
            '+' => tokens.push(Token::new(TokenType::Plus, "+", line)),
            ';' => tokens.push(Token::new(TokenType::Semicolon, ";", line)),
            '*' => tokens.push(Token::new(TokenType::Star, "*", line)),
            '!' => {
                if advance_maybe(&mut current, srclen, &chars, '=') {
                    tokens.push(Token::new(TokenType::Neq, "!=", line));
                } else {
                    tokens.push(Token::new(TokenType::Bang, "!", line));
                }
            },
            '=' => {
                if advance_maybe(&mut current, srclen, &chars, '=') {
                    tokens.push(Token::new(TokenType::Eq, "==", line));
                } else {
                    tokens.push(Token::new(TokenType::Assign, "=", line));
                }
            },
            '<' => {
                if advance_maybe(&mut current, srclen, &chars, '=') {
                    tokens.push(Token::new(TokenType::Leq, "<=", line));
                } else {
                    tokens.push(Token::new(TokenType::Lt, "<", line));
                }
            },
            '>' => {
                if advance_maybe(&mut current, srclen, &chars, '=') {
                    tokens.push(Token::new(TokenType::Geq, ">=", line));
                } else {
                    tokens.push(Token::new(TokenType::Gt, ">", line));
                }
            },
            '/' => {
                let result = advance_maybe(&mut current, srclen, &chars, '/');
                if result {
                    while peek(current, srclen, &chars) != '\n' && !is_end(current, srclen) {
                        advance(&chars, &mut current);
                    }
                } else {
                    tokens.push(Token::new(TokenType::Slash, "/", line));
                }
            },
            ' ' | '\r' | '\t' => {},
            '\n' => { line += 1; },
            '"' => {
                while peek(current, srclen, &chars) != '"' && !is_end(current, srclen) {
                    if peek(current, srclen, &chars) == '\n' {
                        line += 1;
                    }
                    advance(&chars, &mut current);
                }

                if is_end(current, srclen) {
                    // This is a non-recoverable error => early return Err
                    errors.push(LexerError::new(String::from("Unterminated string"), line));
                    let lox_error = consolidate_errors(errors);
                    return Err(lox_error);
                }

                advance(&chars, &mut current);
                let value = &source[start+1..current-1];
                tokens.push(Token::new(TokenType::String, value, line));
            },
            _ => {
                errors.push(LexerError::new(String::from(format!("Unexpected character: {c}")), line));
            }
        }
    }

    if !errors.is_empty() {
        let lox_error = consolidate_errors(errors);
        return Err(lox_error);
    }

    let eof_token = Token::new(TokenType::EOF, "\0", line);
    tokens.push(eof_token);
    Ok(tokens)
}