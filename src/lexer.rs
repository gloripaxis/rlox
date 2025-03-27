use std::{collections::HashMap, error::Error};

use types::{LexerError, Token, TokenType};

use crate::LoxError;

pub mod types;

fn is_end(current: usize, srclen: usize) -> bool {
    current >= srclen
}

fn peek(current: usize, srclen: usize, chars: &[char]) -> char {
    if is_end(current, srclen) {
        return '\0';
    }
    chars[current]
}

fn peek_next(current: usize, srclen: usize, chars: &[char]) -> char {
    if current + 1 > srclen {
        return '\0'
    }
    chars[current+1]
}

fn advance(chars: &[char], current: &mut usize) -> char {
    let c: char = chars[*current];
    *current = *current + 1;
    c
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

fn advance_until(current: &mut usize, line: &mut usize, srclen: usize, chars: &[char], expected: char) {
    while peek(*current, srclen, &chars) != expected && !is_end(*current, srclen) {
        if peek(*current, srclen, &chars) == '\n' {
            *line = *line + 1;
        }
        advance(&chars, current);
    }
}


pub fn scan(source: &str) -> Result<Vec<Token>, Box<dyn Error>> {
    let keywords: HashMap<&str, TokenType> = HashMap::from([
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
        ("while", TokenType::While),
    ]);


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
            // Unambiguous single-symbol tokens
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
            // Ambiguous single-, double-, or multi-symbol tokens
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
                    advance_until(&mut current, &mut line, srclen, &chars, '\n');
                    while peek(current, srclen, &chars) != '\n' && !is_end(current, srclen) {
                        advance(&chars, &mut current);
                    }
                } else {
                    tokens.push(Token::new(TokenType::Slash, "/", line));
                }
            },
            // Whitespace
            ' ' | '\r' | '\t' => {},
            '\n' => { line += 1; },
            // Strings
            '"' => {
                advance_until(&mut current, &mut line, srclen, &chars, '"');
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
            // Numbers
            '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                while peek(current, srclen, &chars).is_ascii_digit() {
                    advance(&chars, &mut current);
                }
                if peek(current, srclen, &chars) == '.' && peek_next(current, srclen, &chars).is_ascii_digit() {
                    advance(&chars, &mut current);
                }
                while peek(current, srclen, &chars).is_ascii_digit() {
                    advance(&chars, &mut current);
                }

                let value = &source[start..current];
                tokens.push(Token::new(TokenType::Number, value, line));
            },
            _ => {
                if !c.is_ascii_alphabetic() {
                    errors.push(LexerError::new(String::from(format!("Unexpected character: {c}")), line));
                    break;
                }
                // Identifiers & Keywords
                while peek(current, srclen, &chars).is_ascii_alphanumeric() {
                    advance(&chars, &mut current);
                }
                
                let value = &source[start..current];
                let ttype = keywords.get(value).copied().unwrap_or(TokenType::Identifier);
                tokens.push(Token::new(ttype, value, line));
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
