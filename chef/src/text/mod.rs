//! Struct for managing references to the source code.

use std::{cmp::max, fs, io, rc::Rc};

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

    pub fn from_spans(start: TextSpan, end: TextSpan) -> Self {
        Self {
            start: start.start,
            end: end.end,
            text: start.text,
        }
    }

    /// Get the text inside the textspan.
    pub fn text(&self) -> &str {
        &self.text.text[self.start..self.end]
    }

    pub fn text_len(&self) -> usize {
        self.end - self.start
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SourceText {
    file: Option<String>,
    text: String,
    lines: Vec<usize>,
}

#[allow(dead_code)]
impl SourceText {
    pub fn from_file(path: &str) -> io::Result<Self> {
        let text = fs::read_to_string(path)?;
        let lines = Self::index_text(&text);
        Ok(Self {
            text,
            lines,
            file: Some(path.to_string()),
        })
    }

    pub fn from_str(string: &str) -> Self {
        Self {
            file: None,
            text: string.to_string(),
            lines: Self::index_text(string),
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

    pub fn file(&self) -> Option<&str> {
        self.file.as_deref()
    }
}
