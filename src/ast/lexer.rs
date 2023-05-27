#[derive(Debug)]
enum TokenKind {
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
pub(crate) struct TokenSpan {
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
pub(crate) struct Token {
    kind: TokenKind,
    span: TokenSpan,
}

impl Token {
    fn new(kind: TokenKind, span: TokenSpan) -> Self {
        Self { kind, span }
    }
}

pub(crate) struct Lexer<'a> {
    input: &'a str,
    position: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer { input, position: 0 }
    }

    fn current(&self) -> char {
        self.input.chars().nth(self.position).unwrap()
    }

    fn is_end(&self) -> bool {
        self.position + 1 >= self.input.len()
    }

    fn consume(&mut self) -> Option<char> {
        if self.is_end() {
            return None
        }
        let c = self.current();
        self.position += 1;
        Some(c)
    }

    fn is_number_start(&self) -> bool {
        self.current().is_digit(10)
    }

    fn consume_number(&mut self) -> u32 {
        let mut n: u32 = 0;
        loop {
            let c = self.current();
            if c.is_digit(10) {
                self.consume().unwrap();
                n = n * 10 + c.to_digit(10).unwrap();
            }
            else {
                break;
            }
        }
        n
    }

    fn is_whitespace(&self) -> bool {
        self.current().is_whitespace()
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
        let start = self.position;
        let kind: TokenKind;
        if self.is_end() {
            return None;
        }
        else if self.is_number_start() {
            kind = TokenKind::Number(self.consume_number());
        }
        else if self.is_whitespace() {
            loop {
                let c = self.current();
                if !c.is_whitespace() { break; }
                self.consume().unwrap();
            }
            kind = TokenKind::Whitespace;
        }
        else {
            kind = self.consume_punctuation();
        }
        let end = self.position;
        let token = Token::new(kind, TokenSpan::new(start, end, self.input[start..end].to_string()));
        Some(token)
    }
}
