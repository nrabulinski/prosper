#[derive(Debug, Copy, Clone)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

type LineCol = (usize, usize);

pub fn line_col(mut pos: usize, input: &str) -> LineCol {
    if pos > input.len() {
        panic!("position out of bounds");
    }

    let slice = &input[..pos];
    let mut chars = slice.chars().peekable();

    let mut line_col = (1, 1);

    while pos != 0 {
        match chars.next() {
            Some('\r') => {
                if let Some(&'\n') = chars.peek() {
                    chars.next();

                    if pos == 1 {
                        pos -= 1;
                    } else {
                        pos -= 2;
                    }

                    line_col = (line_col.0 + 1, 1);
                } else {
                    pos -= 1;
                    line_col = (line_col.0, line_col.1 + 1);
                }
            }
            Some('\n') => {
                pos -= 1;
                line_col = (line_col.0 + 1, 1);
            }
            Some(c) => {
                pos -= c.len_utf8();
                line_col = (line_col.0, line_col.1 + 1);
            }
            None => unreachable!(),
        }
    }

    line_col
}

impl Span {
    pub const EMPTY: Span = Span { start: 0, end: 0 };

    pub fn new(start: usize, end: usize) -> Self {
        Span { start, end }
    }

    pub fn line_col(&self, input: &str) -> (LineCol, LineCol) {
        (line_col(self.start, input), line_col(self.end, input))
    }
    
    pub fn combine(start: Span, end: Span) -> Self {
        Span {
            start: start.start,
            end: end.end
        }
    }

    pub fn is_empty(&self) -> bool {
        self.start == 0 && self.end == 0
    }
}

impl From<std::ops::Range<usize>> for Span {
    fn from(range: std::ops::Range<usize>) -> Self {
        Span::new(range.start, range.end)
    }
}