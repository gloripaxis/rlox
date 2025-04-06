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

impl PartialEq for Pos {
    fn eq(&self, other: &Self) -> bool {
        self.line == other.line && self.col == other.col
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

    pub fn get_prev(&self) -> Self {
        Self {
            line: self.line,
            col: self.col - 1,
        }
    }
}
