mod printer;

use crate::ast::lexer::{TextSpan, TokenKind, Token};
use crate::diagnostics::printer::DiagnosticsPrinter;
use crate::text::SourceText;

use std::fmt::Display;
use std::rc::Rc;
use std::cell::RefCell;

pub type DiagnosticsBagRef = Rc<RefCell<DiagnosticsBag>>;

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string_rep = match self {
            TokenKind::Number(_)    => "number",
            TokenKind::Word(_)      => "word",
            TokenKind::Plus         => "+",
            TokenKind::Minus        => "-",
            TokenKind::Asterisk     => "*",
            TokenKind::Slash        => "/",
            TokenKind::LeftParen    => "(",
            TokenKind::RightParen   => ")",
            TokenKind::LeftSquare   => "[",
            TokenKind::RightSquare  => "]",
            TokenKind::LeftCurly    => "{",
            TokenKind::RightCurly   => "}",
            TokenKind::Equals       => "=",
            TokenKind::Comma        => ",",
            TokenKind::Period       => ".",
            TokenKind::Colon        => ":",
            TokenKind::Semicolon    => ";",
            TokenKind::DoubleEquals => "==",
            TokenKind::RightArrow   => "->",
            TokenKind::Whitespace   => "whitespace",
            TokenKind::Bad          => "bad-token",
            TokenKind::End          => "end-token",
        };
        write!(f, "{}", string_rep)
    }
}

#[derive(Debug)]
pub struct Diagnostic {
    message: String,
    span: TextSpan,
}

impl Diagnostic {
    fn new(message: String, span: TextSpan) -> Self {
        Self { message, span }
    }
}


pub struct DiagnosticsBag {
    diagnostics: Vec<Diagnostic>,
}

impl DiagnosticsBag {
    pub fn new() -> Self {
        Self { diagnostics: vec![] }
    }

    pub fn has_errored(&self) -> bool {
        self.diagnostics.len() > 0
    }

    pub fn report_error(&mut self, token: &Token, message: &str) {
        self.diagnostics.push(Diagnostic::new(message.to_string(), token.span.clone()))
    }

    pub fn report_unexpected_token(&mut self, token: &Token, expected: TokenKind) {
        let message = format!("Expected `{}` but found `{}`.", expected, token.kind);
        self.diagnostics.push(Diagnostic::new(message, token.span.clone()))
    }

    pub fn report_bad_token(&mut self, token: &Token) {
        self.diagnostics.push(Diagnostic::new("Bad token.".to_string(), token.span.clone()))
    }

    pub fn print(&self, source_text: &SourceText) {
        DiagnosticsPrinter::new(source_text, &self.diagnostics).print();
    }
}
