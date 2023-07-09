//! Diagnostics reporting and printing

mod printer;

use crate::ast::lexer::{TokenKind, Token};
use crate::cli::Opts;
use crate::diagnostics::printer::DiagnosticsPrinter;
use crate::text::{SourceText, TextSpan};
use crate::the_chef;

use std::fmt::Display;
use std::rc::Rc;
use std::cell::RefCell;

/// Reference to the [DiagnosticsBag] allowing interior mutability.
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
            TokenKind::DoubleSlash  => "//",
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
            TokenKind::Newline      => "newline",
            TokenKind::Bad          => "bad-token",
            TokenKind::End          => "end-of-program",
        };
        write!(f, "{}", string_rep)
    }
}

/// A chef diagnostic
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

/// A bag holding all the diagnostics with utility functions for reporting and printing errors.
pub struct DiagnosticsBag {
    diagnostics: Vec<Diagnostic>,
    options: Rc<Opts>,
    source: Rc<SourceText>, // TODO: textspans should hold this pointer, not the bag itself, to
                            // allow for multipule files.
}

impl DiagnosticsBag {
    pub fn new(options: Rc<Opts>, source: Rc<SourceText>) -> Self {
        Self { diagnostics: vec![], options, source}
    }

    /// Checks whether any errors have been reported.
    pub fn has_errored(&self) -> bool {
        self.diagnostics.len() > 0
    }

    /// Report an error.
    pub fn report_error(&mut self, span: &TextSpan, message: &str) {
        self.diagnostics.push(Diagnostic::new(message.to_string(), span.clone()))
    }

    /// Report unexpected token error.
    pub fn report_unexpected_token(&mut self, token: &Token, expected: TokenKind) {
        let message = format!("Expected `{}` but found `{}`.", expected, token.kind);
        self.diagnostics.push(Diagnostic::new(message, token.span.clone()))
    }

    /// Report bad token error.
    pub fn report_bad_token(&mut self, token: &Token) {
        self.diagnostics.push(Diagnostic::new("Bad token.".to_string(), token.span.clone()))
    }

    /// Print the accumulated diagnostics.
    pub fn print(&self) {
        DiagnosticsPrinter::new(&self.source, &self.diagnostics).print();
    }

    /// Print the errors and exit.
    pub fn exit_with_errors(&self) {
        println!("\n");
        self.print();
        if !self.options.no_advice {
            println!();
            the_chef::give_advice();
        }
        if cfg!(test) { panic!("Compilation failed.") } // Make tests fail
        std::process::exit(1);
    }

    /// If the diagnostics bag hold any errors: print the errors and exit.
    pub fn exit_if_errored(&self) {
        if self.has_errored() {
            self.exit_with_errors();
        }
    }
}
