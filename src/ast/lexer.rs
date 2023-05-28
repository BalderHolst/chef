use std::path::Path;
use std::fs;
use std::io;

#[derive(Debug, PartialEq)]
pub enum TokenKind {
    Number(u32),
    Word(String),
    Plus,
    Minus,
    Asterisk,
    Slash,
    LeftParen,
    RightParen,
    LeftSquare,
    RightSquare,
    LeftCurly,
    RightCurly,
    Equals,
    Comma,
    Period,
    Colon,
    Semicolon,
    DoubleEquals,
    RightArrow,
    Whitespace,
    Bad,
}

#[derive(Debug)]
pub struct TokenSpan {
    start: usize,
    end: usize,
    text: String,
}

impl TokenSpan {
    fn new(start: usize, end: usize, text: String) -> Self {
        Self { start, end, text }
    }
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: TokenSpan,
}

impl Token {
    fn new(kind: TokenKind, span: TokenSpan) -> Self {
        Self { kind, span }
    }
}

pub(crate) struct Lexer {
    input: String,
    cursor: usize,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Lexer { input: input.to_string(), cursor: 0 }
    }

    pub fn from_file(path: &str) -> io::Result<Self> {
        let input = fs::read_to_string(path)?;
        Ok(Lexer { input, cursor: 0 })
    }

    fn current(&self) -> Option<char> {
        self.input.chars().nth(self.cursor)
    }

    fn consume(&mut self) -> Option<char> {
        let c = self.current();
        self.cursor += 1;
        c
    }

    fn is_number_start(&self) -> Option<bool> {
        Some(self.current()?.is_digit(10))
    }

    fn consume_number(&mut self) -> Option<u32> {
        let mut n: u32 = 0;
        while let Some(c) = self.current() {
            if c.is_digit(10) {
                self.consume().unwrap();
                n = n * 10 + c.to_digit(10).unwrap();
            }
            else {
                break;
            }
        }
        Some(n)
    }

    fn is_whitespace(&self) -> Option<bool> {
        Some(self.current()?.is_whitespace())
    }

    fn consume_punctuation(&mut self) -> TokenKind {
        let first = self.consume().unwrap();
        if self.is_whitespace() != Some(false) || self.is_word_char() == Some(true) {
            match first {
                '+' => TokenKind::Plus,
                '-' => TokenKind::Minus,
                '*' => TokenKind::Asterisk,
                '/' => TokenKind::Slash,
                '(' => TokenKind::LeftParen,
                ')' => TokenKind::RightParen,
                '[' => TokenKind::LeftSquare,
                ']' => TokenKind::RightSquare,
                '{' => TokenKind::LeftCurly,
                '}' => TokenKind::RightCurly,
                '=' => TokenKind::Equals,
                ',' => TokenKind::Comma,
                '.' => TokenKind::Period,
                ':' => TokenKind::Colon,
                ';' => TokenKind::Semicolon,
                _ => TokenKind::Bad,
            }
        }
        else {
            let second = self.consume().unwrap();
            match (first, second) {
                ('=', '=') => TokenKind::DoubleEquals,
                ('-', '>') => TokenKind::RightArrow,
                _ => {
                    dbg!(self.is_word_char());
                    while self.is_whitespace() == Some(true) && self.is_word_char() == Some(false) {
                        self.consume().unwrap();
                    }
                    TokenKind::Bad
                }
            }
        }
    }

    fn is_word_char(&self) -> Option<bool> {
        Some(self.current()?.is_alphabetic())
    }
}

impl Iterator for Lexer {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        let start = self.cursor;
        let kind: TokenKind;
        if self.is_number_start()? {
            let n = self.consume_number()?;
            kind = TokenKind::Number(n);
        }
        else if self.is_whitespace()? {
            while let Some(c) = self.current() {
                if !c.is_whitespace() { break; }
                self.consume().unwrap();
            }
            kind = TokenKind::Whitespace;
        }
        else if self.is_word_char()? {
            let mut word = "".to_string();
            while Some(true) == self.is_word_char() {
                word += self.consume().unwrap().to_string().as_str();
            }
            kind = TokenKind::Word(word);
        }
        else {
            kind = self.consume_punctuation();
        }
        let end = self.cursor;
        let token = Token::new(kind, TokenSpan::new(start, end, self.input[start..end].to_string()));
        Some(token)
    }
}
