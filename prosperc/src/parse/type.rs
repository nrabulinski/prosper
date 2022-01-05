


use super::*;
use crate::{
	token::{Token, TokenKind as TK},
};

impl Type {
	pub fn parse(token: Token) -> Result<SpanType> {
		match token.val {
			TK::Ident(ref s) if is_keyword(s) => Err(ut(token, "type")),
			TK::Ident(s) => {
				let val = match s.as_str() {
					"int" => Type::Primitive(Primitive::Int),
					_ => Type::Udt(s),
				};
				Ok(SpanType {
					val,
					start: token.start,
					end: token.end,
					path: token.path,
				})
			}
			_ => Err(ut(token, "type")),
		}
	}
}
