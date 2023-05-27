#[derive(Debug, PartialEq)]
pub enum TokenKind {
    Number(u32),
    Plus,
    Minus,
    Asterisk,
    Slash,
    LeftParen,
    RightRaren,
    Whitespace,
    End,
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

pub(crate) struct Lexer<'a> {
    input: &'a str,
    cursor: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer { input, cursor: 0 }
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
        match self.consume().unwrap() {
        '+' => TokenKind::Plus,
        '-' => TokenKind::Minus,
        '*' => TokenKind::Asterisk,
        '/' => TokenKind::Slash,
        '(' => TokenKind::LeftParen,
        ')' => TokenKind::RightRaren,
        _ => TokenKind::Bad,
        }
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
        else {
            kind = self.consume_punctuation();
        }
        let end = self.cursor;
        let token = Token::new(kind, TokenSpan::new(start, end, self.input[start..end].to_string()));
        Some(token)
    }
}
