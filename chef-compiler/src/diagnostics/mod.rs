//! Diagnostics reporting and printing

mod printer;

use crate::ast::lexer::{Token, TokenKind};
use crate::cli::Opts;
use crate::diagnostics::printer::DiagnosticsPrinter;
use crate::text::TextSpan;
use crate::the_chef;

use std::cell::RefCell;
use std::rc::Rc;

pub type CompilationResult<T> = std::result::Result<T, CompilationError>;

#[macro_export]
macro_rules! error {
    ($msg:literal $(,)? $($vars:expr),* $(,)?) => {
        $crate::diagnostics::CompilationError::new_generic(format!($msg, $($vars),*))
    };
    ($msg:literal $(,)? $($vars:expr),* $(,)? => $loc:expr) => {
        $crate::diagnostics::CompilationError::new_localized(format!($msg, $($vars),*), $loc)
    };
}

#[derive(Debug, Clone)]
pub struct CompilationError {
    pub desctiption: String,
    pub span: Option<TextSpan>,
}

impl CompilationError {
    pub fn new_localized<S>(desctiption: S, span: TextSpan) -> Self
    where
        S: ToString,
    {
        Self {
            desctiption: desctiption.to_string(),
            span: Some(span),
        }
    }

    pub fn new_generic<S>(desctiption: S) -> Self
    where
        S: ToString,
    {
        Self {
            desctiption: desctiption.to_string(),
            span: None,
        }
    }

    pub fn new_unexpected_token(token: Token, expected: TokenKind) -> Self {
        let desctiption = format!("Expected `{}` but found `{}`.", expected, token.kind);
        Self::new_localized(desctiption, token.span)
    }

    // TODO: Move implementation in diagnostic printer here
    pub fn stringify(&self) -> &str {
        &self.desctiption
    }
}

/// Reference to the [DiagnosticsBag] allowing interior mutability.
pub type DiagnosticsBagRef = Rc<RefCell<DiagnosticsBag>>;

/// A chef diagnostic
#[derive(Debug, Clone)]
pub enum Diagnostic {
    General { message: String },
    Localized { message: String, span: TextSpan },
}

impl Diagnostic {
    fn new_localized(message: String, span: TextSpan) -> Self {
        Self::Localized { message, span }
    }

    fn from_compilation_error(e: CompilationError) -> Self {
        match e.span {
            Some(span) => Self::Localized {
                message: e.desctiption,
                span,
            },
            None => Self::General {
                message: e.desctiption,
            },
        }
    }

    #[allow(unused)]
    pub(crate) fn message(&self) -> &String {
        match self {
            Diagnostic::General { message } => message,
            Diagnostic::Localized { message, span: _ } => message,
        }
    }
}

/// A bag holding all the diagnostics with utility functions for reporting and printing errors
/// within a file.
pub struct DiagnosticsBag {
    diagnostics: Vec<Diagnostic>,
    options: Rc<Opts>,
}

impl DiagnosticsBag {
    pub fn new(options: Rc<Opts>) -> Self {
        Self {
            diagnostics: vec![],
            options,
        }
    }

    pub fn new_ref(options: Rc<Opts>) -> DiagnosticsBagRef {
        let bag = Self::new(options);
        Rc::new(RefCell::new(bag))
    }

    #[cfg(test)]
    pub fn error_count(&self) -> usize {
        self.diagnostics.len()
    }

    /// Checks whether any errors have been reported.
    pub fn has_errored(&self) -> bool {
        !self.diagnostics.is_empty()
    }

    /// Report an error.
    pub fn report_error(&mut self, span: &TextSpan, message: &str) {
        self.diagnostics
            .push(Diagnostic::new_localized(message.to_string(), span.clone()))
    }

    /// Report unexpected token error.
    pub fn _report_unexpected_token(&mut self, token: &Token, expected: TokenKind) {
        let message = format!("Expected `{}` but found `{}`.", expected, token.kind);
        self.diagnostics
            .push(Diagnostic::new_localized(message, token.span.clone()))
    }

    pub fn report_compilation_error(&mut self, error: CompilationError) {
        self.diagnostics
            .push(Diagnostic::from_compilation_error(error))
    }

    /// Print the accumulated diagnostics.
    pub fn print(&self) {
        DiagnosticsPrinter::new(&self.diagnostics).print();
    }

    /// Print the errors and exit.
    pub fn exit_with_errors(&self) {
        println!("\n");
        self.print();
        if !self.options.no_advice {
            println!();
            the_chef::give_advice();
        }
        if cfg!(test) {
            panic!("Compilation failed.")
        } // Make tests fail
        std::process::exit(1);
    }

    /// If the diagnostics bag hold any errors: print the errors and exit.
    pub fn exit_if_errored(&self) {
        if self.has_errored() {
            self.exit_with_errors();
        }
    }
}
