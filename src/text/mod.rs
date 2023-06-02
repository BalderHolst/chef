use std::{fs, io};

pub struct SourceText {
    pub text: String,
    lines: Vec<usize>,
}

impl SourceText {
    pub fn from_file(path: &str) -> io::Result<Self> {
        let text = fs::read_to_string(path)?;
        let lines = Self::index_text(&text);
        Ok(Self { text, lines })
    }

    fn index_text(text: &str) -> Vec<usize> {
        let mut lines: Vec<usize> = vec![0];
        for (i, c) in text.chars().enumerate() {
            if c == '\n' {
                lines.push(i);
            }
        };
        lines
    }

    pub fn get_line_with_position(&self, index: usize) -> (usize, usize) {
        for (line_nr, line_start) in self.lines.iter().enumerate() {
            if &index < line_start {
                return (line_nr, index-self.lines.get(line_nr-1).unwrap());
            }
        };
        (0, 0)
    }

    pub fn get_line(&self, line_nr: usize) -> Option<&str> {
        let start = self.lines.get(line_nr-1)?.clone() + 1;
        let end = self.lines.get(line_nr)?.clone();
        Some(&self.text[start..end])
    }

}
