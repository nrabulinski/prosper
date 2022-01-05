


use super::*;
use crate::{
	token::{Token, TokenKind as TK},
};

impl Ident {
	pub fn parse(token: Token) -> Result<Ident> {
		match token.val {
			TK::Ident(ref s) if is_keyword(s) => Err(ut(token, "identifier")),
			TK::Ident(s) => Ok(Ident {
				val: IdentDef { ident: s },
				start: token.start,
				end: token.end,
				path: token.path,
			}),
			_ => Err(ut(token, "identifier")),
		}
	}
}
