use std::{fs, io, cmp::max};

#[derive(Debug, Clone)]
pub struct TextSpan {
    pub start: usize,
    pub end: usize,
    pub file: String,
}

impl TextSpan {
    pub fn new(start: usize, end: usize, file: String) -> Self {
        Self { start, end, file }
    }

    pub fn text(&self) -> String {
        match fs::read_to_string(&self.file) {
            Ok(s) => s[self.start..self.end].to_string(),
            Err(e) => {
                panic!("{e}");
            },
        }
    }

    pub fn text_len(&self) -> usize {
        dbg!(self.end, self.start);
        self.end - self.start
    }
}


pub struct SourceText {
    pub entry_file: String,
    pub text: String,
    lines: Vec<usize>,
}

impl SourceText {
    pub fn from_file(path: &str) -> io::Result<Self> {
        let text = fs::read_to_string(path)?;
        let lines = Self::index_text(&text);
        Ok(Self { text, lines, entry_file: path.to_string() })
    }

    fn index_text(text: &str) -> Vec<usize> {
        let mut lines: Vec<usize> = vec![0];
        for (i, _) in text.chars().enumerate() {
            if text.chars().nth(max(i as isize - 1, 0) as usize) == Some('\n') {
                lines.push(i);
            }
        };
        lines
    }

    pub fn get_line_nr_and_position(&self, index: usize) -> (usize, usize) {
        for (line_nr, line_start) in self.lines.iter().enumerate() {
            if &index < line_start {
                return (line_nr-1, index-self.lines.get(line_nr-1).unwrap());
            }
        };
        let line_index = self.lines.len()-1;
        (line_index, index - self.lines.get(line_index).unwrap())
    }

    pub fn get_line(&self, line_nr: usize) -> Option<&str> {
        let start = self.lines.get(line_nr)?.clone();
        let end = if let Some(e) = self.lines.get(line_nr+1) {
            e.clone()
        }
        else {
            self.text.len()
        };
        Some(&self.text[start..end])
    }

    pub fn get_file_at(&self, index: usize) -> String {
        self.entry_file.clone()
    }

}
