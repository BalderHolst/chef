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
}

#[derive(Debug, Clone)]
pub struct TextSpan {
    pub start: usize,
    pub end: usize,
    pub text: String,
}

impl TextSpan {
    fn new(start: usize, end: usize, text: String) -> Self {
        Self { start, end, text }
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
    input: &'a str,
    cursor: usize,
    diagnostics_bag: DiagnosticsBagRef,
}

impl<'a> Lexer<'a> {
    pub fn new(diagnostics_bag: DiagnosticsBagRef, input: &'a str) -> Self {
        Lexer { diagnostics_bag, input, cursor: 0 }
    }

    pub fn from_source(diagnostics_bag: DiagnosticsBagRef, text: &'a SourceText) -> Self {
        Lexer { diagnostics_bag, input: &text.text.as_str(), cursor: 0 }
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

    fn is_punctuation_start(&self) -> Option<bool> {
        Some(!self.is_word_char()? && !self.is_whitespace()? && !self.is_number_start()?)
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

    fn is_word_char(&self) -> Option<bool> {
        Some(self.current()?.is_alphabetic())
    }
}

impl<'a> Iterator for Lexer<'a> {
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
        let token = Token::new(kind, TextSpan::new(start, end, self.input[start..end].to_string()));

        if token.kind == TokenKind::Bad {
            self.diagnostics_bag.borrow_mut().report_bad_token(&token);
        }

        Some(token)
    }
}
