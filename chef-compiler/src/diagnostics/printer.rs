use std::cmp::max;
use termion::color::{self, Bg, Fg};

use crate::diagnostics::Diagnostic;
use crate::text::TextSpan;

const PREFIX_LEN: usize = 16;
const MAX_CODE_LEN: usize = 20;
const SUFIX_LEN: usize = 20;

/// Struct for printing diagnostics
pub struct DiagnosticsPrinter<'a> {
    diagnostics: &'a Vec<Diagnostic>,
}

impl<'a> DiagnosticsPrinter<'a> {
    /// Instantiate a new [DiagnosticsPrinter].
    pub fn new(diagnostics: &'a Vec<Diagnostic>) -> Self {
        Self { diagnostics }
    }

    /// Print all the stored diagnostics.
    pub fn print(&self) {
        for diagnostic in self.diagnostics {
            println!("{}", self.stringify_diagnostic(diagnostic));
        }
    }

    /// Turn a diagnostic into a printable message with the following format:
    /// ```text
    /// \[ERROR\] (file:ll:cc) This is the code in question     - Error message
    /// ```
    fn stringify_diagnostic(&self, d: &Diagnostic) -> String {
        let s = match d {
            Diagnostic::General { message } => self.stringify_general_diagnostic(message),
            Diagnostic::Localized { message, span } => {
                self.stringify_localized_diagnostic(message, span)
            }
        };

        format!(
            "{red}[E]{reset} {s}",
            red = Fg(color::Red),
            reset = Fg(color::Reset),
            s = s
        )
    }

    fn stringify_general_diagnostic(&self, message: &String) -> String {
        message.to_string()
    }

    fn stringify_localized_diagnostic(&self, message: &String, span: &TextSpan) -> String {
        // i'm sorry...

        let source = span.text.clone();

        let (line_nr, line_pos) = source.get_line_nr_and_position(span.start);
        let line = source.get_line(line_nr).unwrap();

        let code: String = {
            let code_prefix = {
                if line_pos == 0 {
                    "".to_string()
                } else {
                    let s = &line[0..line_pos];
                    let s = &s[max(0, s.len() as isize - PREFIX_LEN as isize) as usize..];
                    let mut start_whitespace_len: usize = 0;
                    for c in s.chars() {
                        if !c.is_whitespace() {
                            break;
                        }
                        start_whitespace_len += 1;
                    }
                    format!(
                        "{}{}{}{}",
                        Bg(color::White),
                        &s[0..start_whitespace_len],
                        Bg(color::Reset),
                        &s[start_whitespace_len..],
                    )
                }
            };
            let mut actual_code = span.text();
            if actual_code.len() > MAX_CODE_LEN {
                actual_code = &actual_code[0..MAX_CODE_LEN];
            }
            let code_sufix = {
                let start = line_pos as isize + span.text_len() as isize;
                let end = line.len() as isize - 1;
                if start >= end {
                    ""
                } else {
                    let s = &line[start as usize..end as usize];
                    if SUFIX_LEN < s.len() {
                        &s[..SUFIX_LEN]
                    } else {
                        s
                    }
                }
            };
            format!(
                "{}{}{}{}{}",
                code_prefix,
                Fg(color::Red),
                actual_code,
                Fg(color::Reset),
                code_sufix,
            )
            .chars()
            .filter(|c| c != &'\n')
            .collect()
        };

        let location = format!("{}:{}:{}\t", source.file(), line_nr + 1, line_pos + 1);

        let message = format!("{}{}{}", Fg(color::Blue), message, Fg(color::Reset));
        format!("{} {} \t-> {}", location, code, message)
    }
}
