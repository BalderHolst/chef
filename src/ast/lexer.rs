use std::path::Path;
use std::fs;
use std::io;

use crate::diagnostics::DiagnosticsBagRef;
use crate::text::SourceText;

#[derive(Debug, Clone, PartialEq)]
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
    End,
}

#[derive(Debug, Clone)]
pub struct TextSpan {
    pub start: usize,
    pub end: usize,
    pub text: String,
    pub file: String,
}

impl TextSpan {
    fn new(start: usize, end: usize, text: &str, file: String) -> Self {
        Self { start, end, text: text.to_string(), file }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: TextSpan,
}

impl Token {
    fn new(kind: TokenKind, span: TextSpan) -> Self {
        Self { kind, span }
    }
}

pub struct Lexer<'a> {
    source: &'a SourceText,
    cursor: usize,
    diagnostics_bag: DiagnosticsBagRef,
    placed_end_token: bool,
}

impl<'a> Lexer<'a> {
    pub fn from_source(diagnostics_bag: DiagnosticsBagRef, source: &'a SourceText) -> Self {
        Lexer { diagnostics_bag, source, cursor: 0, placed_end_token: false }
    }

    fn current(&self) -> Option<char> {
        self.source.text.chars().nth(self.cursor)
    }

    fn consume(&mut self) -> Option<char> {
        let c = self.current();
        self.cursor += 1;
        c
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

    fn is_number_start(c: char) -> bool {
        c.is_digit(10)
    }

    fn is_word_char(c: char) -> bool {
        c.is_alphabetic()
    }

    fn is_whitespace(c: char) -> bool {
        c.is_whitespace()
    }

    fn is_punctuation_start(c: char) -> bool {
        !Self::is_word_char(c) && !c.is_whitespace() && !Self::is_number_start(c)
    }

    fn consume_punctuation(&mut self) -> TokenKind {
        let first = self.consume().unwrap();
        match first {
            '+' => TokenKind::Plus,
            '-' => {
                match self.current() {
                    Some('>') => {
                        self.consume().unwrap();
                        TokenKind::RightArrow
                    }
                    _ => TokenKind::Minus
                }
            },
            '*' => TokenKind::Asterisk,
            '/' => TokenKind::Slash,
            '(' => TokenKind::LeftParen,
            ')' => TokenKind::RightParen,
            '[' => TokenKind::LeftSquare,
            ']' => TokenKind::RightSquare,
            '{' => TokenKind::LeftCurly,
            '}' => TokenKind::RightCurly,
            '=' => {
                match self.current() {
                    Some('=') => {
                        self.consume().unwrap();
                        TokenKind::DoubleEquals
                    }
                    _ => TokenKind::Equals,
                }
            },
            ',' => TokenKind::Comma,
            '.' => TokenKind::Period,
            ':' => TokenKind::Colon,
            ';' => TokenKind::Semicolon,
            _ => TokenKind::Bad,
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        let start = self.cursor;

        let current_char = if let Some(c) = self.current() { c }
        else if !self.placed_end_token {
            self.placed_end_token = true;
            let pos = self.source.text.len();
            return Some(Token::new(TokenKind::End, TextSpan::new(pos, pos, "", self.source.get_file_at(pos))));
        }
        else {
            return None;
        };
        
        let kind = if Self::is_number_start(current_char) {
            let n = self.consume_number()?;
            TokenKind::Number(n)
        }
        else if Self::is_whitespace(current_char) {
            while let Some(c) = self.current() {
                if !c.is_whitespace() { break; }
                self.consume().unwrap();
            }
            TokenKind::Whitespace
        }
        else if Self::is_word_char(current_char) {
            let mut word = "".to_string();
            while self.current().is_some() && true == Self::is_word_char(self.current().unwrap()) {
                word += self.consume().unwrap().to_string().as_str();
            }
            TokenKind::Word(word)
        }
        else {
            self.consume_punctuation()
        };
        let end = self.cursor;
        let token = Token::new(kind, TextSpan::new(start, end, &self.source.text[start..end], self.source.get_file_at(start)));

        if token.kind == TokenKind::Bad {
            self.diagnostics_bag.borrow_mut().report_bad_token(&token);
        }

        Some(token)
    }
}
