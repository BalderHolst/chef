//! The lexer module converts the raw text into tokens.

use std::fmt::Display;
use std::rc::Rc;

use crate::diagnostics::DiagnosticsBagRef;
use crate::text::{SourceText, TextSpan};

/// Kinds of lexer tokens.
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Number(u32),
    Word(String),
    Plus,
    PlusEquals,
    Minus,
    MinusEquals,
    Asterisk,
    AsteriskEquals,
    Slash,
    SlashEquals,
    LeftParen,
    RightParen,
    LeftSquare,
    RightSquare,
    LeftCurly,
    RightCurly,
    SingleQuote,
    DoubleQuote,
    Equals,
    Comma,
    Period,
    Colon,
    Hashtag,
    Semicolon,
    DoubleEquals,
    RightArrow,
    LargerThan,
    LargerThanEquals,
    LessThan,
    LessThanEquals,
    BangEquals,
    Whitespace,
    Bad,
    End,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string_rep = match self {
            TokenKind::Number(_) => "number",
            TokenKind::Word(_) => "word",
            TokenKind::Plus => "+",
            TokenKind::PlusEquals => "+=",
            TokenKind::Minus => "-",
            TokenKind::MinusEquals => "-=",
            TokenKind::Asterisk => "*",
            TokenKind::AsteriskEquals => "*=",
            TokenKind::Slash => "/",
            TokenKind::SlashEquals => "/=",
            TokenKind::LeftParen => "(",
            TokenKind::RightParen => ")",
            TokenKind::LeftSquare => "[",
            TokenKind::RightSquare => "]",
            TokenKind::LeftCurly => "{",
            TokenKind::RightCurly => "}",
            TokenKind::SingleQuote => "'",
            TokenKind::DoubleQuote => "\"",
            TokenKind::Equals => "=",
            TokenKind::Comma => ",",
            TokenKind::Period => ".",
            TokenKind::Colon => ":",
            TokenKind::Semicolon => ";",
            TokenKind::Hashtag => "#",
            TokenKind::DoubleEquals => "==",
            TokenKind::RightArrow => "->",
            TokenKind::LargerThan => ">",
            TokenKind::LargerThanEquals => ">=",
            TokenKind::LessThan => "<",
            TokenKind::LessThanEquals => "<=",
            TokenKind::BangEquals => "!=",
            TokenKind::Whitespace => "whitespace",
            TokenKind::Bad => "bad-token",
            TokenKind::End => "end-of-program",
        };
        write!(f, "{}", string_rep)
    }
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
/// ```
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
        Lexer {
            diagnostics_bag,
            source,
            cursor: 0,
            placed_end_token: false,
        }
    }

    fn _peak(&self, offset: isize) -> Option<char> {
        let index = self.cursor as isize + offset;
        self.source.text().chars().nth((index) as usize)
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
            if c.is_ascii_digit() {
                self.consume().unwrap();
                n = n * 10 + c.to_digit(10).unwrap();
            } else {
                break;
            }
        }
        Some(n)
    }

    fn is_number_start(c: char) -> bool {
        c.is_ascii_digit()
    }

    fn is_word_char(c: char) -> bool {
        c.is_alphabetic() || c == '_' || c == '-'
    }

    fn is_whitespace(c: char) -> bool {
        c.is_whitespace()
    }

    fn consume_comment(&mut self) {
        loop {
            match self.consume() {
                Some('\n') => {
                    break;
                }
                Some(_) => {}
                None => break,
            }
        }
    }

    // Lexes punktuation. Returns `None` if the punktuation is a commend.
    fn consume_punctuation(&mut self) -> Option<TokenKind> {
        let kind = match self.consume() {
            Some('+') => match self.current() {
                Some('=') => {
                    self.consume();
                    TokenKind::PlusEquals
                }
                _ => TokenKind::Plus,
            },
            Some('-') => match self.current() {
                Some('=') => {
                    self.consume();
                    TokenKind::MinusEquals
                }
                Some('>') => {
                    self.consume();
                    TokenKind::RightArrow
                }
                _ => TokenKind::Minus,
            },
            Some('*') => match self.current() {
                Some('=') => {
                    self.consume();
                    TokenKind::AsteriskEquals
                }
                _ => TokenKind::Asterisk,
            },
            Some('/') => match self.current() {
                Some('=') => {
                    self.consume();
                    TokenKind::SlashEquals
                }
                Some('/') => {
                    self.consume_comment();
                    return None;
                }
                _ => TokenKind::Slash,
            },
            Some('(') => TokenKind::LeftParen,
            Some(')') => TokenKind::RightParen,
            Some('[') => TokenKind::LeftSquare,
            Some(']') => TokenKind::RightSquare,
            Some('{') => TokenKind::LeftCurly,
            Some('}') => TokenKind::RightCurly,
            Some('\'') => TokenKind::SingleQuote,
            Some('"') => TokenKind::DoubleQuote,
            Some('=') => match self.consume() {
                Some('=') => TokenKind::DoubleEquals,
                _ => {
                    self.backtrack(1);
                    TokenKind::Equals
                }
            },
            Some(',') => TokenKind::Comma,
            Some('.') => TokenKind::Period,
            Some(':') => TokenKind::Colon,
            Some(';') => TokenKind::Semicolon,
            Some('#') => TokenKind::Hashtag,
            Some('>') => match self.consume() {
                Some('=') => TokenKind::LargerThanEquals,
                _ => {
                    self.backtrack(1);
                    TokenKind::LargerThan
                }
            },
            Some('<') => match self.consume() {
                Some('=') => TokenKind::LessThanEquals,
                _ => {
                    self.backtrack(1);
                    TokenKind::LessThan
                }
            },
            Some('!') => match self.consume() {
                Some('=') => TokenKind::BangEquals,
                _ => {
                    self.backtrack(1);
                    TokenKind::Bad
                }
            },
            _ => TokenKind::Bad,
        };
        Some(kind)
    }
}

impl Iterator for Lexer {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        let start = self.cursor;

        let current_char = if let Some(c) = self.current() {
            c
        } else if !self.placed_end_token {
            self.placed_end_token = true;
            let pos = self.source.text().len();
            return Some(Token::new(
                TokenKind::End,
                TextSpan::new(pos, pos, self.source.clone()),
            ));
        } else {
            return None;
        };

        let kind = if Self::is_number_start(current_char) {
            let n = self.consume_number()?;
            TokenKind::Number(n)
        } else if Self::is_whitespace(current_char) {
            while let Some(c) = self.current() {
                if !Self::is_whitespace(c) {
                    break;
                }
                self.consume().unwrap();
            }
            TokenKind::Whitespace
        } else if Self::is_word_char(current_char) && current_char != '-' {
            let mut word = "".to_string();
            while self.current().is_some() && Self::is_word_char(self.current().unwrap()) {
                word += self.consume().unwrap().to_string().as_str();
            }
            TokenKind::Word(word)
        } else {
            match self.consume_punctuation() {
                Some(kind) => kind,
                None => return self.next(), // Ignore and get next if punktuation was a comment
            }
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
        let diagnostics_bag = Rc::new(std::cell::RefCell::new(
            crate::diagnostics::DiagnosticsBag::new(
                Rc::new(crate::cli::Opts::new_test()),
                text.clone(),
            ),
        ));
        let lexer = Self::from_source(diagnostics_bag.clone(), text.clone());
        (text, diagnostics_bag, lexer)
    }

    pub fn _new_dummy() -> Self {
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
    let code = "==->+=-=*=/=";
    let expected_tokens = vec![
        TokenKind::DoubleEquals,
        TokenKind::RightArrow,
        TokenKind::PlusEquals,
        TokenKind::MinusEquals,
        TokenKind::AsteriskEquals,
        TokenKind::SlashEquals,
        TokenKind::End,
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
