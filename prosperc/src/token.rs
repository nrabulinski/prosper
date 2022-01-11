use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[rustfmt::skip]
pub enum Token {
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

    // Keywords
    Fun,
    Extern,
}

impl fmt::Display for Token {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		use Token::*;
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
			Fun => write!(f, "fun"),
			Extern => write!(f, "extern"),
		}
	}
}

// TODO: Literal kinds (64bit unsigned int, 32bit float etc)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

impl From<Literal> for Token {
	fn from(lit: Literal) -> Self {
		Token::Lit(lit)
	}
}
