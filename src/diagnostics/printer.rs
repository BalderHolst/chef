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
        let (line_nr, line_pos) = self.source_text.get_line_nr_and_position(d.span.start);
        let line = self.source_text.get_line(line_nr).unwrap();

        let code: String = {
            let code_prefix = {
                if line_pos == 0 {
                    "".to_string()
                }
                else {
                    let s = &line[0..line_pos];
                    let s = &s[max(0, s.len() as isize - PREFIX_LEN as isize) as usize..];
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
                }
            };
            let code_sufix = {
                let start = line_pos as isize +d.span.text.len() as isize;
                let end = line.len() as isize -1;
                if start >= end  {
                    ""
                }
                else {
                    let s = &line[start as usize..end as usize];
                    if SUFIX_LEN < s.len() { &s[..SUFIX_LEN] }
                    else { s }
                }
            };
            format!(
                "{}{}{}{}{}",
                code_prefix,
                Fg(color::Red),
                d.span.text,
                Fg(color::Reset),
                code_sufix,
            )
                .chars()
                .filter(|c| c != &'\n')
                .collect()
        };
        let location = format!("{}[E]{} {}:{}:{}\t", Fg(color::Red), Fg(color::Reset), d.span.file, line_nr+1, line_pos+1);
        let message = format!("{}{}{}", Fg(color::Blue), d.message, Fg(color::Reset));
        format!("{} {} \t-> {}", location, code, message)
    }
}

