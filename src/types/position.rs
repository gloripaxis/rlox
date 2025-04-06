use std::fmt;

#[derive(Debug, Clone, Copy)]
pub struct Pos {
    pub line: usize,
    pub col: usize,
}

impl fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "line {}, column {}", self.line, self.col)
    }
}

impl Pos {
    pub fn new(line: usize, col: usize) -> Self {
        Self { line, col }
    }

    pub fn newline(&mut self, col: Option<usize>) {
        self.line += 1;
        self.col = col.unwrap_or(1);
    }

    pub fn next(&mut self) {
        self.col += 1;
    }
}
