use if_chain::if_chain;


use super::*;
use crate::{
	token::{TokenKind as TK},
};

impl Function {
	pub fn parse(tokens: &mut Stream) -> Result<Function> {
		let fun = tokens.next().unwrap()?;
		let start = fun.start;
		let ident = tokens.next().ok_or_else(|| eof("identifier"))??;
		let ident = Ident::parse(ident)?;
		let mut lp = tokens.next().ok_or_else(|| eof("(, ="))??;
		let mut args = Vec::new();
		if let TK::LParen = *lp {
			loop {
				let ident = tokens
					.peek()
					.ok_or_else(|| eof("), identifier"))?
					.as_ref()
					.map_err(|err| ParseError::from(err.clone()))?;
				let ident = if_chain! {
					if let TK::Ident(_) = &**ident;
					if let Ok(ident) = Ident::parse(tokens.next().unwrap()?);
					then { ident }
					else { break; }
				};
				let colon = tokens.next().ok_or_else(|| eof(":"))??;
				if !matches!(*colon, TK::Colon) {
					return Err(ut(colon, ":"));
				}
				let ty = tokens.next().ok_or_else(|| eof("type"))??;
				let ty = Type::parse(ty)?;
				args.push((ident, ty));
				let comma = tokens
					.peek()
					.ok_or_else(|| eof(r#"), ",""#))?
					.as_ref()
					.map_err(|err| ParseError::from(err.clone()))?;
				if !matches!(&**comma, TK::Comma) {
					break;
				}
				let _ = tokens.next();
			}
			let rp = tokens.next().ok_or_else(|| eof("), identifier"))??;
			if !matches!(*rp, TK::RParen) {
				return Err(ut(rp, "), identifier"));
			}
			lp = tokens.next().ok_or_else(|| eof("="))??;
		}
		if !matches!(*lp, TK::Eq) {
			return Err(ut(lp, "="));
		}
		let body = Expr::parse(tokens)?;
		let end = if body.requires_semi() {
			let semi = tokens.next().ok_or_else(|| eof(";"))??;
			if !matches!(*semi, TK::Semi) {
				return Err(ut(semi, ";"));
			}
			semi.end
		} else {
			body.end
		};
		let path = body.path.clone();
		Ok(FunctionDef {
			name: ident,
			args,
			ret: SpanType {
				val: Type::Primitive(Primitive::Unit),
				start: lp.end,
				end: body.start,
				path: path.clone(),
			},
			body,
		}
		.span(start, end, path))
	}
}
