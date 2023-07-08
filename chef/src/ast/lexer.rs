//! The lexer module converts the raw text into tokens.

use std::collections::HashSet;
use std::rc::Rc;

use lazy_static::lazy_static;

use crate::diagnostics::DiagnosticsBagRef;
use crate::text::{SourceText, TextSpan};

lazy_static! {
    static ref PUNCTUATION_CHARS: HashSet<char> = 
        HashSet::from(
            [ '+', '-', '>', '*', '/', '(', ')', '[', ']', '{', '}', '=', ',', '.', ':', ';', ]
            );
}

/// Kinds of lexer tokens.
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

/// A lexer token.
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

/// The lexer. Tokens can be extracting by iterating over it.
/// ```rust, run
/// # let lexer = Lexer::new_dummy();
/// for token in lexer {
///     /* do something */ 
/// }
/// ```
pub struct Lexer {
    source: Rc<SourceText>,
    cursor: usize,
    diagnostics_bag: DiagnosticsBagRef,
    placed_end_token: bool,
}

impl Lexer {

    /// Instantiate a [Lexer] with a [SourceText] for parsing and a [DiagnosticsBagRef] for
    /// reporting errors.
    pub fn from_source(diagnostics_bag: DiagnosticsBagRef, source: Rc<SourceText>) -> Self {
        Lexer { diagnostics_bag, source, cursor: 0, placed_end_token: false }
    }

    fn _peak(&self, offset: isize) -> Option<char> {
        self.source.text().chars().nth((self.cursor as isize + offset) as usize)
    }

    fn current(&self) -> Option<char> {
        self.source.text().chars().nth(self.cursor)
    }

    fn consume(&mut self) -> Option<char> {
        let c = self.current();
        self.cursor += 1;
        c
    }

    fn backtrack(&mut self, n: usize) {
        self.cursor -= n;
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

    fn _is_punctuation_char(c: &char) -> bool {
        PUNCTUATION_CHARS.get(c).is_some()
    }

    fn consume_punctuation(&mut self) -> TokenKind {
        match self.consume() {
            Some('+') => TokenKind::Plus,
            Some('-') => {
                match self.consume() {
                    Some('>') => TokenKind::RightArrow,
                    _ => {
                        self.backtrack(1);
                        TokenKind::Minus
                    },
                }
            },
            Some('*') => TokenKind::Asterisk,
            Some('/') => TokenKind::Slash,
            Some('(') => TokenKind::LeftParen,
            Some(')') => TokenKind::RightParen,
            Some('[') => TokenKind::LeftSquare,
            Some(']') => TokenKind::RightSquare,
            Some('{') => TokenKind::LeftCurly,
            Some('}') => TokenKind::RightCurly,
            Some('=') => {
                match self.consume() {
                    Some('=') => TokenKind::DoubleEquals,
                    _ => {
                        self.backtrack(1);
                        TokenKind::Equals
                    },
                }
            },
            Some(',') => TokenKind::Comma,
            Some('.') => TokenKind::Period,
            Some(':') => TokenKind::Colon,
            Some(';') => TokenKind::Semicolon,
            _ => TokenKind::Bad,
        }
    }
}

impl Iterator for Lexer {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        let start = self.cursor;

        let current_char = if let Some(c) = self.current() { c }
        else if !self.placed_end_token {
            self.placed_end_token = true;
            let pos = self.source.text().len();
            return Some(Token::new(TokenKind::End, TextSpan::new(pos, pos, self.source.clone())));
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
        let token = Token::new(kind, TextSpan::new(start, end, self.source.clone()));


        if token.kind == TokenKind::Bad {
            self.diagnostics_bag.borrow_mut().report_bad_token(&token);
        }

        Some(token)
    }
}

#[cfg(test)]
impl Lexer {
    pub fn new_bundle(s: &str) -> (Rc<SourceText>, DiagnosticsBagRef, Self) {
        let text = Rc::new(SourceText::from_str(s));
        let diagnostics_bag = Rc::new(std::cell::RefCell::new(crate::diagnostics::DiagnosticsBag::new(Rc::new(crate::cli::Opts::default()), text.clone())));
        let lexer = Self::from_source(diagnostics_bag.clone(), text.clone());
        (text, diagnostics_bag, lexer)
    }

    pub fn new_dummy() -> Self {
        let (_source, _diagnostics_bag, lexer) = Lexer::new_bundle("");
        lexer
    }
}

#[test]
fn lex_string() {
    let code = "1 +3       -> 10;\n (5 / 4);";
    let expected_tokens = vec![
        TokenKind::Number(1),
        TokenKind::Whitespace,
        TokenKind::Plus,
        TokenKind::Number(3),
        TokenKind::Whitespace,
        TokenKind::RightArrow,
        TokenKind::Whitespace,
        TokenKind::Number(10),
        TokenKind::Semicolon,
        TokenKind::Whitespace,
        TokenKind::LeftParen,
        TokenKind::Number(5),
        TokenKind::Whitespace,
        TokenKind::Slash,
        TokenKind::Whitespace,
        TokenKind::Number(4),
        TokenKind::RightParen,
        TokenKind::Semicolon,
        TokenKind::End,
    ];
    let (_text, _diagnostics_bag, lexer) = Lexer::new_bundle(code);
    let lexed_tokens: Vec<TokenKind> = lexer.map(|t| t.kind).collect();
    assert_eq!(lexed_tokens, expected_tokens);
}

#[test]
fn lex_2_char_operators() {
    let code = "==->-=";
    let expected_tokens = vec![
        TokenKind::DoubleEquals,
        TokenKind::RightArrow,
        TokenKind::Minus,
        TokenKind::Equals,
        TokenKind::End
    ];
    let (_text, _diagnostics_bag, lexer) = Lexer::new_bundle(code);
    let lexed_tokens: Vec<TokenKind> = lexer.map(|t| t.kind).collect();
    assert_eq!(lexed_tokens, expected_tokens);
}

#[test]
fn lex_all_tokens() {
    let code = "10hello+-*/()[]{}=,.:;==->  @";
    let expected_tokens = vec![
        TokenKind::Number(10),
        TokenKind::Word("hello".to_string()),
        TokenKind::Plus,
        TokenKind::Minus,
        TokenKind::Asterisk,
        TokenKind::Slash,
        TokenKind::LeftParen,
        TokenKind::RightParen,
        TokenKind::LeftSquare,
        TokenKind::RightSquare,
        TokenKind::LeftCurly,
        TokenKind::RightCurly,
        TokenKind::Equals,
        TokenKind::Comma,
        TokenKind::Period,
        TokenKind::Colon,
        TokenKind::Semicolon,
        TokenKind::DoubleEquals,
        TokenKind::RightArrow,
        TokenKind::Whitespace,
        TokenKind::Bad,
        TokenKind::End,
    ];
    let (_text, _diagnostics_bag, lexer) = Lexer::new_bundle(code);
    let lexed_tokens: Vec<TokenKind> = lexer.map(|t| t.kind).collect();
    assert_eq!(lexed_tokens, expected_tokens);
}
