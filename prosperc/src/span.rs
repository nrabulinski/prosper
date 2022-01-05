use std::{fmt, ops::Deref};

#[derive(Debug, Clone, Copy)]
pub struct Pos {
	pub line: usize,
	pub col: usize,
}

impl fmt::Display for Pos {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}:{}", self.line, self.col)
	}
}

#[derive(Debug, Clone)]
pub struct Spanned<T> {
	pub val: T,
	pub path: String,
	pub start: Pos,
	pub end: Pos,
}

impl<T> Deref for Spanned<T> {
	type Target = T;

	fn deref(&self) -> &T {
		&self.val
	}
}

impl<T: fmt::Display> fmt::Display for Spanned<T> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{} at {}:{}", self.val, self.path, self.start)
	}
}
