use termion::color::{self, Fg, Bg};
use std::cmp::{min, max};

use crate::text::SourceText;
use crate::diagnostics::Diagnostic;

const PREFIX_LEN: usize = 16;
const SUFIX_LEN: usize = 20;

pub struct DiagnosticsPrinter<'a> {
    source_text: &'a SourceText,
    diagnostics: &'a Vec<Diagnostic>
}

impl<'a> DiagnosticsPrinter<'a> {
    pub fn new(source_text: &'a SourceText, diagnostics: &'a Vec<Diagnostic>) -> Self {
        Self { source_text, diagnostics }
    }

    pub fn print(&self) {
        for diagnostic in self.diagnostics {
            println!("{}", self.stringify_diagnostic(diagnostic));
        }
    }

    /// [ERROR] (file:ll:cc) This is the code in question     - Error message
    fn stringify_diagnostic(&self, d: &Diagnostic) -> String {
        let (line_nr, line_pos) = self.source_text.get_line_with_position(d.span.start);
        let line = self.source_text.get_line(line_nr).unwrap();
        let code = {
            let code_prefix = {
                let mut s = &line[0..line_pos-1];
                s = &s[max(0, s.len() as isize - PREFIX_LEN as isize) as usize..];
                let mut start_whitespace_len: usize = 0;
                for c in s.chars() {
                    if !c.is_whitespace() { break; }
                    start_whitespace_len += 1;
                }
                format!("{}{}{}{}", 
                        Bg(color::White),
                        &s[0..start_whitespace_len],
                        Bg(color::Reset),
                        &s[start_whitespace_len..],
                        )
            };
            let code_sufix = {
                let s = &line[line_pos-1+d.span.text.len()..];
                if SUFIX_LEN < s.len() { &s[..SUFIX_LEN] }
                else { s }
            };
            format!(
                "{}{}{}{}{}",
                code_prefix,
                Fg(color::Red),
                d.span.text,
                Fg(color::Reset),
                code_sufix,
            )
        };
        let location = format!("{}[E]{} {}:{}:{}\t", Fg(color::Red), Fg(color::Reset), "file", line_nr, line_pos);
        let message = format!("{}{}{}", Fg(color::Blue), d.message, Fg(color::Reset));
        format!("{} {} \t-> {}", location, code, message)
    }
}

