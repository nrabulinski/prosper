

use thiserror::Error;

use crate::{
	span::{Pos, Spanned},
	token::{Literal::*, Token, TokenKind::*},
};

#[derive(Error, Debug, Clone)]
pub enum LexError {
	#[error("Unexpected character {0:?} at position {1}")]
	UnexpectedChar(char, Pos),
}

pub type Result = std::result::Result<Token, LexError>;

pub fn parse<'a>(path: &'a str, source: &'a str) -> impl Iterator<Item = Result> + 'a {
	macro_rules! tok {
		($val:expr, $start:expr, $end:expr) => {
			Spanned {
				val: $val.into(),
				path: path.to_string(),
				start: $start,
				end: $end,
			}
		};
	}
	let mut line = 1;
	let mut col = 1;
	let mut iter = source.chars().peekable();
	std::iter::from_fn(move || loop {
		let ch = iter.next()?;
		let start = Pos { line, col };
		let mut end = start;
		col += 1;
		match ch {
			'[' => return Some(Ok(tok!(LBracket, start, end))),
			']' => return Some(Ok(tok!(RBracket, start, end))),

			'(' => return Some(Ok(tok!(LParen, start, end))),
			')' => return Some(Ok(tok!(RParen, start, end))),

			';' => return Some(Ok(tok!(Semi, start, end))),

			',' => return Some(Ok(tok!(Comma, start, end))),

			':' => return Some(Ok(tok!(Colon, start, end))),

			'=' => return Some(Ok(tok!(Eq, start, end))),

			'-' => {
				if iter.next_if_eq(&'>').is_some() {
					end.col = col;
					col += 1;
					return Some(Ok(tok!(ThinArrow, start, end)));
				} else {
					return Some(Ok(tok!(Dash, start, end)));
				}
			}

			'/' => {
				if iter.next_if_eq(&'/').is_some() {
					let mut comment = String::new();
					while let Some(c) = iter.next_if(|&c| c != '\n') {
						col += 1;
						comment.push(c);
					}
					end.col = col - 1;
					return Some(Ok(tok!(Comment(comment), start, end)));
				} else {
					return Some(Ok(tok!(Slash, start, end)));
				}
			}

			c if c.is_ascii_digit() => {
				let mut num = String::from(c);
				while let Some(c) = iter.next_if(|&c| c.is_ascii_digit()) {
					col += 1;
					num.push(c)
				}
				end.col = col - 1;
				return Some(Ok(tok!(Int { val: num }, start, end)));
			}

			c if c.is_alphabetic() => {
				let mut ident = String::from(c);
				while let Some(c) = iter.next_if(|&c| c.is_alphanumeric() || c == '_') {
					col += 1;
					ident.push(c)
				}
				end.col = col - 1;
				return Some(Ok(tok!(Ident(ident), start, end)));
			}

			'\n' => {
				line += 1;
				col = 1;
			}
			c if c.is_ascii_whitespace() => continue,
			c => return Some(Err(LexError::UnexpectedChar(c, start))),
		}
	})
}
