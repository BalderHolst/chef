//! Struct for managing references to the source code.

use std::{cmp::max, fs, rc::Rc};

use crate::{
    ast::python_macro::run_python_import, cli::Opts, diagnostics::CompilationResult, error,
};

#[derive(Debug, Clone, PartialEq)]
pub struct TextSpan {
    pub(crate) start: usize,
    pub(crate) end: usize,
    pub(crate) text: Rc<SourceText>,
}

impl TextSpan {
    pub fn new(start: usize, end: usize, text: Rc<SourceText>) -> Self {
        Self { start, end, text }
    }

    pub fn from_spans(start: &TextSpan, end: &TextSpan) -> Self {
        Self {
            start: start.start,
            end: end.end,
            text: start.text.clone(),
        }
    }

    /// Get the text inside the text span.
    pub fn text(&self) -> &str {
        &self.text.text[self.start..self.end]
    }

    pub fn text_len(&self) -> usize {
        self.end - self.start
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SourceText {
    file: String,
    text: String,
    lines: Vec<usize>,
}

#[allow(dead_code)]
impl SourceText {
    pub fn new(path: String, contents: String) -> Self {
        let lines = Self::index_text(contents.as_str());
        Self {
            file: path,
            text: contents,
            lines,
        }
    }

    pub fn from_file(path: &str, opts: Rc<Opts>) -> CompilationResult<Self> {
        if path.ends_with(".py") {
            return run_python_import(opts, None, path, None, None);
        };

        let text =
            fs::read_to_string(path).map_err(|e| error!("Error reading file `{}`: {}", path, e))?;

        let lines = Self::index_text(&text);
        Ok(Self {
            text,
            lines,
            file: path.to_string(),
        })
    }

    #[cfg(test)]
    pub fn from_str(text: &str) -> Self {
        let lines = Self::index_text(text);
        Self {
            file: "test".to_string(),
            text: text.to_string(),
            lines,
        }
    }

    fn index_text(text: &str) -> Vec<usize> {
        let mut lines: Vec<usize> = vec![0];
        for (i, _) in text.chars().enumerate() {
            if text.chars().nth(max(i as isize - 1, 0) as usize) == Some('\n') {
                lines.push(i);
            }
        }
        lines
    }

    pub fn get_line_nr_and_position(&self, index: usize) -> (usize, usize) {
        for (line_nr, line_start) in self.lines.iter().enumerate() {
            if &index < line_start {
                return (line_nr - 1, index - self.lines.get(line_nr - 1).unwrap());
            }
        }
        let line_index = self.lines.len() - 1;
        (line_index, index - self.lines.get(line_index).unwrap())
    }

    pub fn get_line(&self, line_nr: usize) -> Option<&str> {
        let start = *self.lines.get(line_nr)?;
        let end = if let Some(e) = self.lines.get(line_nr + 1) {
            *e
        } else {
            self.text.len()
        };
        Some(&self.text[start..end])
    }

    pub fn text(&self) -> &str {
        self.text.as_ref()
    }

    pub fn file(&self) -> &str {
        &self.file
    }
}
