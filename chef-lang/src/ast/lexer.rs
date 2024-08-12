//! The lexer module converts the raw text into tokens.

use std::fmt::Display;
use std::rc::Rc;

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
    LeftDoubleArrow,
    LeftCurlyArrow,
    LeftCurlyDoubleArrow,
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
    Bar,
    Bad,
    End,
    Comment(String),
}

impl TokenKind {
    pub fn is_assignment_operator(&self) -> bool {
        matches!(self, TokenKind::LeftArrow | TokenKind::LeftCurlyArrow)
    }

    pub fn is_comment(&self) -> bool {
        matches!(self, TokenKind::Comment(_))
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
            TokenKind::LeftDoubleArrow => "<<-",
            TokenKind::LeftCurlyArrow => "<~",
            TokenKind::LeftCurlyDoubleArrow => "<<~",
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
            TokenKind::Bar => "|",
            TokenKind::Bad => "bad-token",
            TokenKind::End => "end-of-program",
            TokenKind::Comment(c) => format!("// {c}").leak(),
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
    chars: Vec<char>,
    cursor: usize,
    placed_end_token: bool,
}

impl Lexer {
    /// Instantiate a [Lexer] with a [SourceText] for parsing and a [DiagnosticsBagRef] for
    /// reporting errors.
    pub fn from_source(source: Rc<SourceText>) -> Self {
        let chars = source.text().chars().collect();
        Lexer {
            source,
            chars,
            cursor: 0,
            placed_end_token: false,
        }
    }

    fn peak(&self, offset: isize) -> Option<char> {
        let index = self.cursor as isize + offset;
        self.chars.get(index as usize).copied()
    }

    fn current(&self) -> Option<char> {
        self.peak(0)
    }

    fn consume(&mut self) -> Option<char> {
        let c = self.current();
        self.cursor += 1;
        c
    }

    fn backtrack(&mut self, n: usize) {
        self.cursor -= n;
    }

    fn consume_expected_chars(&mut self, expected: &str) {
        for (i, c) in expected.chars().enumerate() {
            if self.consume() != Some(c) {
                panic!("Expected chars \"{expected}\" but found '{c}' as char number {i}.");
            }
        }
    }

    fn consume_whitespace(&mut self) {
        while let Some(c) = self.current() {
            if !Self::is_whitespace(c) {
                break;
            }
            self.consume().unwrap();
        }
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

    /// Consumes a comment and returns it as a string. This method changes the cursor position.
    fn consume_comment(&mut self) -> String {
        let start = self.cursor;
        self.consume_expected_chars("//");
        self.consume_whitespace();
        let mut comment = String::new();
        loop {
            match self.consume() {
                Some('\n') => {
                    break;
                }
                Some(c) => comment.push(c),
                None => break,
            }
        }
        comment
    }

    /// Lexes punctuation.
    fn consume_punctuation(&mut self) -> Option<TokenKind> {
        #[rustfmt::skip]
        let (len, kind) = match (self.peak(0), self.peak(1), self.peak(2)) {
            (Some('+'), Some('='), _,       ) => (2, TokenKind::PlusEquals),
            (Some('+'), _,         _,       ) => (1, TokenKind::Plus),
            (Some('-'), Some('='), _,       ) => (2, TokenKind::MinusEquals),
            (Some('-'), Some('>'), _,       ) => (2, TokenKind::RightArrow),
            (Some('-'), _,         _,       ) => (1, TokenKind::Minus),
            (Some('*'), Some('='), _,       ) => (2, TokenKind::AsteriskEquals),
            (Some('*'), _,         _,       ) => (1, TokenKind::Asterisk),
            (Some('/'), Some('='), _,       ) => (2, TokenKind::SlashEquals),
            (Some('/'), Some('/'), _,       ) => (0, TokenKind::Comment(self.consume_comment())),
            (Some('/'), _,         _,       ) => (1, TokenKind::Slash),
            (Some('('), _,         _,       ) => (1, TokenKind::LeftParen),
            (Some(')'), _,         _,       ) => (1, TokenKind::RightParen),
            (Some('['), _,         _,       ) => (1, TokenKind::LeftSquare),
            (Some(']'), _,         _,       ) => (1, TokenKind::RightSquare),
            (Some('{'), _,         _,       ) => (1, TokenKind::LeftCurly),
            (Some('}'), _,         _,       ) => (1, TokenKind::RightCurly),
            (Some('|'), _,         _,       ) => (1, TokenKind::Bar),
            (Some('\''),_,         _,       ) => (1, TokenKind::SingleQuote),
            (Some('='), Some('='), _,       ) => (2, TokenKind::DoubleEquals),
            (Some('='), Some('>'), _,       ) => (2, TokenKind::RightFatArrow),
            (Some('='), _,         _,       ) => (1, TokenKind::Equals),
            (Some(','), _,         _,       ) => (1, TokenKind::Comma),
            (Some('.'), _,         _,       ) => (1, TokenKind::Period),
            (Some(':'), _,         _,       ) => (1, TokenKind::Colon),
            (Some(';'), _,         _,       ) => (1, TokenKind::Semicolon),
            (Some('#'), _,         _,       ) => (1, TokenKind::Hashtag),
            (Some('@'), Some('='), Some('=')) => (3, TokenKind::EveryDoubleEquals),
            (Some('@'), Some('>'), Some('=')) => (3, TokenKind::EveryLargerThanEquals),
            (Some('@'), Some('>'), _,       ) => (2, TokenKind::EveryLargerThan),
            (Some('@'), Some('<'), Some('=')) => (3, TokenKind::EveryLessThanEquals),
            (Some('@'), Some('<'), _,       ) => (2, TokenKind::EveryLessThan),
            (Some('@'), Some('!'), Some('=')) => (3, TokenKind::EveryBangEquals),
            (Some('@'), _,         _,       ) => (1, TokenKind::At),
            (Some('?'), Some('='), Some('=')) => (3, TokenKind::AnyDoubleEquals),
            (Some('?'), Some('>'), Some('=')) => (3, TokenKind::AnyLargerThanEquals),
            (Some('?'), Some('>'), _,       ) => (2, TokenKind::AnyLargerThan),
            (Some('?'), Some('<'), Some('=')) => (3, TokenKind::AnyLessThanEquals),
            (Some('?'), Some('<'), _,       ) => (2, TokenKind::AnyLessThan),
            (Some('?'), Some('!'), Some('=')) => (3, TokenKind::AnyBangEquals),
            (Some('?'), _,         _,       ) => (1, TokenKind::QuestionMark),
            (Some('>'), Some('='), _,       ) => (2, TokenKind::LargerThanEquals),
            (Some('>'), _,         _,       ) => (1, TokenKind::LargerThan),
            (Some('<'), Some('='), _,       ) => (2, TokenKind::LessThanEquals),
            (Some('<'), Some('-'), _,       ) => (2, TokenKind::LeftArrow),
            (Some('<'), Some('<'), Some('-')) => (3, TokenKind::LeftDoubleArrow),
            (Some('<'), Some('~'), _,       ) => (2, TokenKind::LeftCurlyArrow),
            (Some('<'), Some('<'), Some('~')) => (3, TokenKind::LeftCurlyDoubleArrow),
            (Some('<'), _,         _,       ) => (1, TokenKind::LessThan),
            (Some('!'), Some('='), _,       ) => (2, TokenKind::BangEquals),
            (Some('&'), Some('&'), _,       ) => (2, TokenKind::DoubleAnd),
            (Some('&'), _,         _,       ) => (1, TokenKind::And),
            _ => (1, TokenKind::Bad),
        };
        self.cursor += len;
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
    pub fn new_bundle(s: &str) -> (Rc<SourceText>, crate::diagnostics::DiagnosticsBagRef, Self) {
        let text = Rc::new(SourceText::from_str(s));
        let diagnostics_bag = Rc::new(std::cell::RefCell::new(
            crate::diagnostics::DiagnosticsBag::new(Rc::new(crate::cli::Opts::new_test())),
        ));
        let lexer = Self::from_source(text.clone());
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
    let code = "10hello+-*/()[]{}=,.:;==&->&&  @@==@!=@>@>=@<@<=?==?!=?>?>=?<?<='\"literal'\"";
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
        TokenKind::EveryDoubleEquals,
        TokenKind::EveryBangEquals,
        TokenKind::EveryLargerThan,
        TokenKind::EveryLargerThanEquals,
        TokenKind::EveryLessThan,
        TokenKind::EveryLessThanEquals,
        TokenKind::AnyDoubleEquals,
        TokenKind::AnyBangEquals,
        TokenKind::AnyLargerThan,
        TokenKind::AnyLargerThanEquals,
        TokenKind::AnyLessThan,
        TokenKind::AnyLessThanEquals,
        TokenKind::SingleQuote,
        TokenKind::StringLiteral("literal'".to_string()),
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
