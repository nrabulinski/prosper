use thiserror::Error;

use crate::{
	ast::*,
	lex::{self, LexError},
	token::Token,
};

mod expr;
mod func_def;
mod function;
mod ident;
mod item;
mod r#type;

#[derive(Error, Debug, Clone)]
pub enum ParseError {
	#[error(transparent)]
	Lex(#[from] LexError),
	#[error("Unexpected token {got}{}{}", if expected.is_empty() { "" } else { ", expected " }, .expected)]
	UnexpectedToken { got: Token, expected: &'static str },
	#[error("Unexpected end of file{}{}", if expected.is_empty() { "" } else { ", expected " }, .expected)]
	Eof { expected: &'static str },
}

pub type Result<T> = std::result::Result<T, ParseError>;

type Stream<'a> = std::iter::Peekable<Box<dyn Iterator<Item = lex::Result> + 'a>>;

pub fn parse<'a>(
	tokens: impl IntoIterator<Item = lex::Result> + 'a,
) -> impl Iterator<Item = Result<Item>> + 'a {
	let iter: Box<dyn Iterator<Item = lex::Result> + 'a> = Box::new(tokens.into_iter());
	let mut iter = iter.peekable();
	std::iter::from_fn(move || {
		iter.peek()?;
		Some(Item::parse(&mut iter))
	})
}

fn ut(got: Token, expected: &'static str) -> ParseError {
	ParseError::UnexpectedToken { got, expected }
}

fn eof(expected: &'static str) -> ParseError {
	ParseError::Eof { expected }
}

fn is_keyword(s: &str) -> bool {
	matches!(s, "fun" | "extern")
}
