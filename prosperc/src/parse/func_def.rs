use if_chain::if_chain;


use super::*;
use crate::{
	token::{TokenKind as TK},
};

impl FuncDef {
	pub fn parse(tokens: &mut Stream) -> Result<FuncDef> {
		let fun = tokens.next().unwrap()?;
		let start = fun.start;
		let ident = tokens.next().ok_or_else(|| eof("identifier"))??;
		let ident = Ident::parse(ident)?;
		let lp = tokens.next().ok_or_else(|| eof("("))??;
		if !matches!(*lp, TK::LParen) {
			return Err(ut(lp, "("));
		}
		let mut args = Vec::new();
		loop {
			let ty = tokens
				.peek()
				.ok_or_else(|| eof("), type"))?
				.as_ref()
				.map_err(|err| ParseError::from(err.clone()))?;
			if_chain! {
				if let TK::Ident(_) = &**ty;
				if let Ok(ty) = Type::parse(tokens.next().unwrap()?);
				then { args.push(ty); }
				else { break; }
			}
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
		let rp = tokens.next().ok_or_else(|| eof(r#"), ",""#))??;
		if !matches!(*rp, TK::RParen) {
			return Err(ut(rp, r#"), ",""#));
		}
		let mut arr = tokens.next().ok_or_else(|| eof(";, ->"))??;
		let ret = if let TK::ThinArrow = *arr {
			let ty = tokens.next().ok_or_else(|| eof("type"))??;
			let ty = Type::parse(ty)?;
			arr = tokens.next().ok_or_else(|| eof(";"))??;
			Some(ty)
		} else {
			None
		};
		if !matches!(*arr, TK::Semi) {
			return Err(ut(rp, ";"));
		}
		let end = arr.end;
		Ok(FuncDefDef {
			name: ident,
			args,
			ret: ret.unwrap_or_else(|| SpanType {
				val: Type::Primitive(Primitive::Unit),
				start: rp.end,
				end: arr.start,
				path: arr.path.clone(),
			}),
		}
		.span(start, end, arr.path))
	}
}
