use chumsky::prelude::*;

use crate::{
	span::Spanned,
	token::{Literal, Token},
};

pub fn lex() -> impl Parser<char, Vec<Spanned<Token>>, Error = Simple<char>> {
	let int_b10 = text::int(10)
		.map(|val| Literal::Int { val })
		.map(Token::Lit);

	let lbracket = just('[').to(Token::LBracket);
	let rbracket = just(']').to(Token::RBracket);

	let lparen = just('(').to(Token::LParen);
	let rparen = just(')').to(Token::RParen);

	let semi = just(';').to(Token::Semi);

	let colon = just(':').to(Token::Colon);

	let comma = just(',').to(Token::Comma);

	let thin_arrow = just("->").to(Token::ThinArrow);

	let fat_arrow = just("=>").to(Token::FatArrow);

	let eq = just('=').to(Token::Eq);

	let dash = just('-').to(Token::Dash);

	let comment = just("//").then(take_until(just('\n'))).padded();

	let slash = just('/').to(Token::Slash);

	let ident = text::ident().map(|ident: String| match ident.as_str() {
		"fun" => Token::Fun,
		"extern" => Token::Extern,
		_ => Token::Ident(ident),
	});

	let token = choice((
		int_b10, lbracket, rbracket, lparen, rparen, semi, colon, comma, thin_arrow, fat_arrow, eq,
		dash, slash, ident,
	))
	.recover_with(skip_then_retry_until([]));

	token
		.padded_by(comment.repeated())
		.map_with_span(|tok, span| (tok, span))
		.padded()
		.repeated()
}
