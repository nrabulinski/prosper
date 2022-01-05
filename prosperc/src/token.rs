use std::{fmt};

use crate::span::Spanned;

pub type Token = Spanned<TokenKind>;

#[derive(Debug, Clone)]
#[rustfmt::skip]
pub enum TokenKind {
	LBracket, RBracket,
	LParen, RParen,

	Semi, Comma, Colon,

    ThinArrow, // ->
    FatArrow, // =>

	Eq,
    Dash,
	Slash,

	Ident(String),

	Lit(Literal),

	Comment(String),
}

impl fmt::Display for TokenKind {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		use TokenKind::*;
		match self {
			LBracket => write!(f, "["),
			RBracket => write!(f, "]"),
			LParen => write!(f, "("),
			RParen => write!(f, ")"),
			Semi => write!(f, ";"),
			Comma => write!(f, ","),
			Colon => write!(f, ":"),
			ThinArrow => write!(f, "->"),
			FatArrow => write!(f, "=>"),
			Eq => write!(f, "="),
			Dash => write!(f, "-"),
			Slash => write!(f, "/"),
			Ident(s) => write!(f, "ident: {:?}", s),
			Lit(lit) => write!(f, "{:?}", lit),
			Comment(s) => write!(f, "comment: {:?}", s),
		}
	}
}

// TODO: Literal kinds (64bit unsigned int, 32bit float etc)
#[derive(Debug, Clone)]
pub enum Literal {
	Int {
		val: String,
		// kind: Option<IntKind>
	},

	Float {
		val: String,
	},

	Str {
		val: String,
	},
}

impl From<Literal> for TokenKind {
	fn from(lit: Literal) -> Self {
		TokenKind::Lit(lit)
	}
}
