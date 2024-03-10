//! The lexer module converts the raw text into tokens.

use std::fmt::Display;
use std::rc::Rc;

use crate::diagnostics::DiagnosticsBagRef;
use crate::text::{SourceText, TextSpan};

/// Kinds of lexer tokens.
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Number(u16),
    Word(String),
    StringLiteral(String),
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
    At,
    Period,
    Colon,
    Hashtag,
    Bang,
    Semicolon,
    And,
    DoubleAnd,
    RightArrow,
    RightFatArrow,
    LeftArrow,
    LeftCurlyArrow,
    DoubleEquals,
    LargerThan,
    LargerThanEquals,
    LessThan,
    LessThanEquals,
    BangEquals,
    EveryDoubleEquals,
    EveryLargerThan,
    EveryLargerThanEquals,
    EveryLessThan,
    EveryLessThanEquals,
    EveryBangEquals,
    AnyDoubleEquals,
    AnyLargerThan,
    AnyLargerThanEquals,
    AnyLessThan,
    AnyLessThanEquals,
    AnyBangEquals,
    Whitespace,
    QuestionMark,
    Bad,
    End,
}

impl TokenKind {
    pub fn is_assignment_operator(&self) -> bool {
        matches!(self, TokenKind::LeftArrow | TokenKind::LeftCurlyArrow)
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string_rep = match self {
            TokenKind::Number(_) => "number",
            TokenKind::Word(_) => "word",
            TokenKind::StringLiteral(_) => "literal",
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
            TokenKind::Bang => "!",
            TokenKind::At => "@",
            TokenKind::DoubleEquals => "==",
            TokenKind::And => "&",
            TokenKind::DoubleAnd => "&&",
            TokenKind::RightArrow => "->",
            TokenKind::RightFatArrow => "=>",
            TokenKind::LeftArrow => "<-",
            TokenKind::LeftCurlyArrow => "<~",
            TokenKind::LargerThan => ">",
            TokenKind::LargerThanEquals => ">=",
            TokenKind::LessThan => "<",
            TokenKind::LessThanEquals => "<=",
            TokenKind::BangEquals => "!=",
            TokenKind::EveryDoubleEquals => "@==",
            TokenKind::EveryLargerThan => "@>",
            TokenKind::EveryLargerThanEquals => "@>=",
            TokenKind::EveryLessThan => "@<",
            TokenKind::EveryLessThanEquals => "@<=",
            TokenKind::EveryBangEquals => "@!=",
            TokenKind::AnyDoubleEquals => "?==",
            TokenKind::AnyLargerThan => "?>",
            TokenKind::AnyLargerThanEquals => "?>=",
            TokenKind::AnyLessThan => "?<",
            TokenKind::AnyLessThanEquals => "?<=",
            TokenKind::AnyBangEquals => "?!=",
            TokenKind::QuestionMark => "?",
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

    pub(crate) fn is_end(&self) -> bool {
        matches!(self.kind, TokenKind::End)
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

    fn consume_number(&mut self) -> Option<u16> {
        let mut n_str = String::new();
        while let Some(c) = self.consume() {
            if c.is_ascii_digit() {
                n_str.push(c);
            } else {
                break;
            }
        }
        self.backtrack(1);
        let n = n_str.parse().unwrap();
        Some(n)
    }

    fn is_number_start(c: Option<char>) -> bool {
        match c {
            Some(c) => c.is_ascii_digit(),
            None => false,
        }
    }

    fn is_word_start_char(c: char) -> bool {
        c.is_alphabetic() || c == '_'
    }

    fn is_word_char(c: char) -> bool {
        Self::is_word_start_char(c) || c.is_numeric() || c == '-'
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

    /// Lexes punctuation. Returns `None` if the punctuation is a comment.
    fn consume_punctuation(&mut self) -> Option<TokenKind> {
        let kind = match self.consume() {
            Some('+') => match self.current() {
                Some('=') => {
                    self.consume();
                    TokenKind::PlusEquals
                }
                _ => TokenKind::Plus,
            },
            Some('-') => match self.consume() {
                Some('=') => TokenKind::MinusEquals,
                Some('>') => TokenKind::RightArrow,
                _ => {
                    self.backtrack(1);
                    TokenKind::Minus
                }
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
                Some('>') => TokenKind::RightFatArrow,
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
            Some('@') => match self.consume() {
                Some('=') => match self.consume() {
                    Some('=') => TokenKind::EveryDoubleEquals,
                    _ => {
                        self.backtrack(2);
                        TokenKind::At
                    }
                },
                Some('>') => match self.consume() {
                    Some('=') => TokenKind::EveryLargerThanEquals,
                    _ => {
                        self.backtrack(1);
                        TokenKind::EveryLargerThan
                    }
                },
                Some('<') => match self.consume() {
                    Some('=') => TokenKind::EveryLessThanEquals,
                    _ => {
                        self.backtrack(1);
                        TokenKind::EveryLessThan
                    }
                },
                Some('!') => match self.consume() {
                    Some('=') => TokenKind::EveryBangEquals,
                    _ => {
                        self.backtrack(2);
                        TokenKind::At
                    }
                },
                _ => {
                    self.backtrack(1);
                    TokenKind::At
                }
            },
            Some('?') => match self.consume() {
                Some('=') => match self.consume() {
                    Some('=') => TokenKind::AnyDoubleEquals,
                    _ => {
                        self.backtrack(2);
                        TokenKind::At
                    }
                },
                Some('>') => match self.consume() {
                    Some('=') => TokenKind::AnyLargerThanEquals,
                    _ => {
                        self.backtrack(1);
                        TokenKind::AnyLargerThan
                    }
                },
                Some('<') => match self.consume() {
                    Some('=') => TokenKind::AnyLessThanEquals,
                    _ => {
                        self.backtrack(1);
                        TokenKind::AnyLessThan
                    }
                },
                Some('!') => match self.consume() {
                    Some('=') => TokenKind::AnyBangEquals,
                    _ => {
                        self.backtrack(2);
                        TokenKind::QuestionMark
                    }
                },
                _ => {
                    self.backtrack(1);
                    TokenKind::QuestionMark
                }
            },
            Some('>') => match self.consume() {
                Some('=') => TokenKind::LargerThanEquals,
                _ => {
                    self.backtrack(1);
                    TokenKind::LargerThan
                }
            },
            Some('<') => match self.consume() {
                Some('=') => TokenKind::LessThanEquals,
                Some('-') => TokenKind::LeftArrow,
                Some('~') => TokenKind::LeftCurlyArrow,
                _ => {
                    self.backtrack(1);
                    TokenKind::LessThan
                }
            },
            Some('!') => match self.consume() {
                Some('=') => TokenKind::BangEquals,
                _ => {
                    self.backtrack(1);
                    TokenKind::Bang
                }
            },
            Some('&') => match self.consume() {
                Some('&') => TokenKind::DoubleAnd,
                _ => {
                    self.backtrack(1);
                    TokenKind::And
                }
            },
            _ => TokenKind::Bad,
        };
        Some(kind)
    }

    fn consume_literal(&mut self) -> TokenKind {
        self.consume(); // Consume '"'

        let mut text = String::new();

        while let Some(c) = self.consume() {
            if c == '"' {
                break;
            }
            text.push(c)
        }

        TokenKind::StringLiteral(text)
    }

    fn consume_word(&mut self) -> TokenKind {
        let mut word = String::new();
        while self.current().is_some() && Self::is_word_char(self.current().unwrap()) {
            word += self.consume().unwrap().to_string().as_str();
        }
        TokenKind::Word(word)
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

        let kind = match current_char {
            '"' => self.consume_literal(),
            c if Self::is_number_start(Some(c)) => {
                let n = self.consume_number()?;
                TokenKind::Number(n)
            }
            c if Self::is_whitespace(c) => {
                while let Some(c) = self.current() {
                    if !Self::is_whitespace(c) {
                        break;
                    }
                    self.consume().unwrap();
                }
                TokenKind::Whitespace
            }
            c if Self::is_word_start_char(c) => self.consume_word(),
            _ => match self.consume_punctuation() {
                Some(kind) => kind,
                None => return self.next(), // Ignore and get next if punktuation was a comment
            },
        };

        let end = self.cursor;
        let token = Token::new(kind, TextSpan::new(start, end, self.source.clone()));

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
fn lex_literal() {
    let c = r#"
this is a "literal"
        "#;

    let expected_tokens = vec![
        TokenKind::Whitespace,
        TokenKind::Word("this".to_string()),
        TokenKind::Whitespace,
        TokenKind::Word("is".to_string()),
        TokenKind::Whitespace,
        TokenKind::Word("a".to_string()),
        TokenKind::Whitespace,
        TokenKind::StringLiteral("literal".to_string()),
        TokenKind::Whitespace,
        TokenKind::End,
    ];
    let (_text, _diagnostics_bag, lexer) = Lexer::new_bundle(c);
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
    let code = "10hello+-*/()[]{}=,.:;==&->&&  @";
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
        TokenKind::And,
        TokenKind::RightArrow,
        TokenKind::DoubleAnd,
        TokenKind::Whitespace,
        TokenKind::At,
        TokenKind::End,
    ];
    let (_text, _diagnostics_bag, lexer) = Lexer::new_bundle(code);
    let lexed_tokens: Vec<TokenKind> = lexer.map(|t| t.kind).collect();
    assert_eq!(lexed_tokens, expected_tokens);
}

#[test]
fn lex_words_with_numbers() {
    let code = "th1s_is_0ne_single_w0rd42_7";
    let expected_tokens = vec![
        TokenKind::Word("th1s_is_0ne_single_w0rd42_7".to_string()),
        TokenKind::End,
    ];
    let (_text, _diagnostics_bag, lexer) = Lexer::new_bundle(code);
    let lexed_tokens: Vec<TokenKind> = lexer.map(|t| t.kind).collect();
    assert_eq!(lexed_tokens, expected_tokens);
}

#[test]
fn lex_negative_numbers_as_minuses() {
    let code = "-4 30 -30 1234 -1234";
    let expected_tokens = vec![
        TokenKind::Minus,
        TokenKind::Number(4),
        TokenKind::Whitespace,
        TokenKind::Number(30),
        TokenKind::Whitespace,
        TokenKind::Minus,
        TokenKind::Number(30),
        TokenKind::Whitespace,
        TokenKind::Number(1234),
        TokenKind::Whitespace,
        TokenKind::Minus,
        TokenKind::Number(1234),
        TokenKind::End,
    ];
    let (_text, _diagnostics_bag, lexer) = Lexer::new_bundle(code);
    let lexed_tokens: Vec<TokenKind> = lexer.map(|t| t.kind).collect();
    assert_eq!(lexed_tokens, expected_tokens);
}

#[test]
fn test_many_operations() {
    let code = "1 @== 2?> 3 ?< 4 ?<=5 ?!= 6";
    let expected_tokens = vec![
        TokenKind::Number(1),
        TokenKind::Whitespace,
        TokenKind::EveryDoubleEquals,
        TokenKind::Whitespace,
        TokenKind::Number(2),
        TokenKind::AnyLargerThan,
        TokenKind::Whitespace,
        TokenKind::Number(3),
        TokenKind::Whitespace,
        TokenKind::AnyLessThan,
        TokenKind::Whitespace,
        TokenKind::Number(4),
        TokenKind::Whitespace,
        TokenKind::AnyLessThanEquals,
        TokenKind::Number(5),
        TokenKind::Whitespace,
        TokenKind::AnyBangEquals,
        TokenKind::Whitespace,
        TokenKind::Number(6),
        TokenKind::End,
    ];

    let (_text, _diagnostics_bag, lexer) = Lexer::new_bundle(code);
    let lexed_tokens: Vec<TokenKind> = lexer.map(|t| t.kind).collect();
    assert_eq!(lexed_tokens, expected_tokens);
}
