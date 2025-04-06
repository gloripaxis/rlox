use std::rc::Rc;

use crate::errors::LoxError;
use crate::types::position::Pos;
use crate::types::{literal::Lit, token::Token, token::TokenType};

pub struct Lexer<'a> {
    start: usize,
    current: usize,

    start_pos: Pos,
    cur_pos: Pos,

    source: &'a str,
    chars: Vec<char>,
    tokens: Vec<Token>,

    errors: Vec<LoxError>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        let chars: Vec<char> = source.chars().collect();

        Self {
            start: 0,
            current: 0,
            start_pos: Pos::new(1, 1),
            cur_pos: Pos::new(1, 1),
            source,
            chars,
            tokens: Vec::new(),
            errors: Vec::new(),
        }
    }

    pub fn scan(mut self) -> Result<Vec<Token>, Vec<LoxError>> {
        while !self.is_end() {
            self.start = self.current;
            self.start_pos = self.cur_pos;
            self.scan_token();
        }

        if !self.errors.is_empty() {
            return Err(self.errors);
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
                true => self.advance_comment(),
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
            _ => self
                .errors
                .push(LoxError::illegal_character(self.cur_pos.get_prev(), c)),
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
            self.errors
                .push(LoxError::unterminated_string(self.cur_pos, self.start_pos));
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
        self.current >= self.source.len()
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

    fn advance_comment(&mut self) {
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
                    '\n' => self.cur_pos.newline(Some(0)),
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
                    _ => self.errors.push(LoxError::invalid_escape(self.cur_pos, self.peek())),
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
        if self.current + 1 > self.source.len() {
            return '\0';
        }
        self.chars[self.current + 1]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tokens_for(source: &'static str) -> Vec<Token> {
        Lexer::new(source).scan().unwrap()
    }

    fn match_tokens_with_types_and_literals(tokens: Vec<Token>, types: &[TokenType], literals: &[Lit]) {
        assert_eq!(tokens.len(), types.len() + 1);
        assert_eq!(tokens.len(), literals.len() + 1);
        for ((t, tt), l) in tokens.iter().zip(types).zip(literals) {
            assert_eq!(t.get_type(), *tt);
            match (&l, &t.get_literal()) {
                (Lit::Id(x), Lit::Id(y)) => assert_eq!(x, y),
                _ => assert_eq!(*l, t.get_literal()),
            }
        }
        assert_eq!(tokens.last().unwrap().get_type(), TokenType::EndOfFile);
    }

    #[test]
    fn correct_single_char_symbols() {
        let source = "( ) { } , . ; - + *";
        let exp_tt = [
            TokenType::LeftParen,
            TokenType::RightParen,
            TokenType::LeftBrace,
            TokenType::RightBrace,
            TokenType::Comma,
            TokenType::Dot,
            TokenType::Semicolon,
            TokenType::Minus,
            TokenType::Plus,
            TokenType::Star,
        ];
        let exp_lit = [
            Lit::Nil,
            Lit::Nil,
            Lit::Nil,
            Lit::Nil,
            Lit::Nil,
            Lit::Nil,
            Lit::Nil,
            Lit::Nil,
            Lit::Nil,
            Lit::Nil,
        ];
        match_tokens_with_types_and_literals(tokens_for(source), &exp_tt, &exp_lit);
    }

    #[test]
    fn correct_ambiguous_symbols() {
        let source = "/ = ! != == > >= < <=";
        let exp_tt = [
            TokenType::Slash,
            TokenType::Assign,
            TokenType::Bang,
            TokenType::Neq,
            TokenType::Eq,
            TokenType::Gt,
            TokenType::Geq,
            TokenType::Lt,
            TokenType::Leq,
        ];
        let exp_lit = [
            Lit::Nil,
            Lit::Nil,
            Lit::Nil,
            Lit::Nil,
            Lit::Nil,
            Lit::Nil,
            Lit::Nil,
            Lit::Nil,
            Lit::Nil,
        ];
        match_tokens_with_types_and_literals(tokens_for(source), &exp_tt, &exp_lit);
    }

    #[test]
    fn correct_identifiers() {
        let source = "myvar myfunc var2 for2 whiletrue class2";
        let exp_tt = [
            TokenType::Identifier,
            TokenType::Identifier,
            TokenType::Identifier,
            TokenType::Identifier,
            TokenType::Identifier,
            TokenType::Identifier,
        ];
        let exp_lit = [
            Lit::Id(Rc::from(String::from("myvar"))),
            Lit::Id(Rc::from(String::from("myfunc"))),
            Lit::Id(Rc::from(String::from("var2"))),
            Lit::Id(Rc::from(String::from("for2"))),
            Lit::Id(Rc::from(String::from("whiletrue"))),
            Lit::Id(Rc::from(String::from("class2"))),
        ];
        match_tokens_with_types_and_literals(tokens_for(source), &exp_tt, &exp_lit);
    }

    #[test]
    fn correct_strings() {
        let source = "\"value\" \"myname\\\"is\" \"my\\\\name\" \"abc\ndef\" \"abc\n\"";
        println!("{source}");
        let exp_tt = [
            TokenType::String,
            TokenType::String,
            TokenType::String,
            TokenType::String,
            TokenType::String,
        ];
        let exp_lit = [
            Lit::Str(Rc::from(String::from("value"))),
            Lit::Str(Rc::from(String::from("myname\"is"))),
            Lit::Str(Rc::from(String::from("my\\name"))),
            Lit::Str(Rc::from(String::from("abc\ndef"))),
            Lit::Str(Rc::from(String::from("abc\n"))),
        ];
        match_tokens_with_types_and_literals(tokens_for(source), &exp_tt, &exp_lit);
    }

    #[test]
    fn correct_numbers() {
        let source = "12.34 12.00 17";
        let exp_tt = [TokenType::Number, TokenType::Number, TokenType::Number];
        let exp_lit = [Lit::Num(12.34), Lit::Num(12.00), Lit::Num(17.00)];
        match_tokens_with_types_and_literals(tokens_for(source), &exp_tt, &exp_lit);
    }

    #[test]
    fn correct_bools() {
        let source = "true false";
        let exp_tt = [TokenType::True, TokenType::False];
        let exp_lit = [Lit::Bool(true), Lit::Bool(false)];
        match_tokens_with_types_and_literals(tokens_for(source), &exp_tt, &exp_lit);
    }

    #[test]
    fn correct_keywords() {
        let source = "and class else fun for if nil or print return super this var while";
        let exp_tt = [
            TokenType::And,
            TokenType::Class,
            TokenType::Else,
            TokenType::Fun,
            TokenType::For,
            TokenType::If,
            TokenType::Nil,
            TokenType::Or,
            TokenType::Print,
            TokenType::Return,
            TokenType::Super,
            TokenType::This,
            TokenType::Var,
            TokenType::While,
        ];
        let exp_lit = [
            Lit::Nil,
            Lit::Nil,
            Lit::Nil,
            Lit::Nil,
            Lit::Nil,
            Lit::Nil,
            Lit::Nil,
            Lit::Nil,
            Lit::Nil,
            Lit::Nil,
            Lit::Nil,
            Lit::Nil,
            Lit::Nil,
            Lit::Nil,
        ];
        match_tokens_with_types_and_literals(tokens_for(source), &exp_tt, &exp_lit);
    }

    #[test]
    fn correct_comments() {
        let source = "var a = 10; //should be ignored";
        let tokens = tokens_for(source);
        assert_eq!(tokens.len(), 6);
        assert_eq!(tokens.get(4).unwrap().get_type(), TokenType::Semicolon);
    }

    #[test]
    fn whitespace_ignored() {
        let source = "   \r \n \t   ";
        let tokens = Lexer::new(source).scan().unwrap();
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens.last().unwrap().get_type(), TokenType::EndOfFile);
    }

    #[test]
    fn illegal_string_escape() {
        let source = r#" "value\evalue" "#;
        let result = Lexer::new(source).scan();
        match result {
            Ok(_) => panic!("Expected invalid escape sequence"),
            Err(e) => {
                assert_eq!(e.len(), 1);
                let err = e.first().unwrap();
                assert_eq!(
                    "ParseError at line 1, column 9: Invalid escape sequence '\\e'",
                    format!("{err}")
                );
            }
        }
    }

    #[test]
    fn unexpected_character() {
        let source = "&";
        let result = Lexer::new(source).scan();
        match result {
            Ok(_) => panic!("Expected error 'Encountered illegal character'"),
            Err(errors) => {
                assert_eq!(errors.len(), 1);
                assert_eq!(
                    format!("{}", errors.first().unwrap()),
                    "ParseError at line 1, column 1: Encountered illegal character '&'"
                );
            }
        }
    }

    #[test]
    fn unterminated_string() {
        let source = "var x = 10;\nvar str = \"abcdef\nghijkl\nmno";
        let result = Lexer::new(source).scan();
        match result {
            Ok(_) => panic!("Expected error: 'Unterminated string'"),
            Err(errors) => {
                assert_eq!(errors.len(), 1);
                assert_eq!(
                    format!("{}", errors.first().unwrap()),
                    "ParseError at line 4, column 4: Unterminated string starting at line 2, column 11"
                );
            }
        }
    }

    #[test]
    fn tricky_symbols() {
        let source = ">== === ==!==== 2//b";
        let exp_tt = [
            TokenType::Geq,
            TokenType::Assign,
            TokenType::Eq,
            TokenType::Assign,
            TokenType::Eq,
            TokenType::Neq,
            TokenType::Eq,
            TokenType::Assign,
            TokenType::Number,
        ];
        let exp_lit = [
            Lit::Nil,
            Lit::Nil,
            Lit::Nil,
            Lit::Nil,
            Lit::Nil,
            Lit::Nil,
            Lit::Nil,
            Lit::Nil,
            Lit::Num(2.0),
        ];
        match_tokens_with_types_and_literals(tokens_for(source), &exp_tt, &exp_lit);
    }

    #[test]
    fn position_tracking() {
        let source = r#"
var x = "abc
def";
var y = 8; // comment ignored
var z = true;
"#;
        let tokens = Lexer::new(source).scan().unwrap();
        let positions = [
            Pos::new(2, 1),
            Pos::new(2, 5),
            Pos::new(2, 7),
            Pos::new(2, 9),
            Pos::new(3, 5),
            Pos::new(4, 1),
            Pos::new(4, 5),
            Pos::new(4, 7),
            Pos::new(4, 9),
            Pos::new(4, 10),
            Pos::new(5, 1),
            Pos::new(5, 5),
            Pos::new(5, 7),
            Pos::new(5, 9),
            Pos::new(5, 13),
        ];
        for (t, p) in tokens.iter().zip(positions) {
            assert_eq!(t.get_position(), p);
        }
    }
}
