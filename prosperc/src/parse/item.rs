use if_chain::if_chain;


use super::*;
use crate::{
	token::{TokenKind as TK},
};

impl Item {
	pub fn parse(tokens: &mut Stream) -> Result<Item> {
		// SAFETY: We check that the stream is non-empty in `parse`
		let tok = tokens
			.peek()
			.unwrap()
			.as_ref()
			.map_err(|err| ParseError::from(err.clone()))?;

		if let TK::Ident(s) = &**tok {
			match s.as_str() {
				"fun" => Ok(Item::Func(Box::new(Function::parse(tokens)?))),
				"extern" => Ok(Item::Extern(parse_extern(tokens)?)),
				_ => Err(ut(tok.clone(), "fun, extern")),
			}
		} else {
			Err(ut(tok.clone(), "fun, extern"))
		}
	}
}

fn parse_extern(tokens: &mut Stream) -> Result<Vec<FuncDef>> {
	// SAFETY: We checked that the stream is non-empty in `parse_item`
	let _ext = tokens.next().unwrap()?;
	let lb = tokens.next().ok_or_else(|| eof("["))??;
	if !matches!(*lb, TK::LBracket) {
		return Err(ut(lb, "["));
	}
	let mut res = Vec::new();
	loop {
		let fun = tokens
			.peek()
			.ok_or_else(|| eof("], function definition"))?
			.as_ref()
			.map_err(|err| ParseError::from(err.clone()))?;
		if_chain! {
			if let TK::Ident(s) = &**fun;
			if s == "fun";
			then {
				res.push(FuncDef::parse(tokens)?);
			}
			else { break; }
		}
	}
	let rb = tokens.next().ok_or_else(|| eof("], function definition"))??;
	if !matches!(*rb, TK::RBracket) {
		return Err(ut(rb, "], function definition"));
	}
	Ok(res)
}
