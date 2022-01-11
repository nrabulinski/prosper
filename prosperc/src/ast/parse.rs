use chumsky::prelude::*;

use super::*;
use crate::token::Token;

pub fn parser() -> impl Parser<Token, Vec<Spanned<Item>>, Error = Simple<Token>> {
	parse_func()
		.map(Item::Function)
		.or(parse_extern_block())
		.map_with_span(|item, span| (item, span))
		.repeated()
}

fn parse_type() -> impl Parser<Token, Type, Error = Simple<Token>> {
	filter_map(|span, tok| match tok {
		Token::Ident(ident) => Ok(match ident.as_str() {
			"int" => Type::int(),
			_ => Type::Udt(ident.clone()),
		}),
		_ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
	})
}

fn parse_func_def() -> impl Parser<Token, Spanned<FuncDef>, Error = Simple<Token>> {
	let ident = filter_map(|span, tok| match tok {
		Token::Ident(ident) => Ok(ident),
		_ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
	});

	let args = ident
		.then_ignore(just(Token::Colon))
		.or_not()
		.then(parse_type())
		.map_with_span(|v, span| (v, span))
		.separated_by(just(Token::Comma))
		.allow_trailing()
		.delimited_by(Token::LParen, Token::RParen)
		.labelled("function arguments");

	let ret = just(Token::ThinArrow)
		.ignore_then(parse_type())
		.or_not()
		.map(|ret| ret.unwrap_or(Type::unit()));

	just(Token::Fun)
		.ignore_then(ident)
		.then(args)
		.then(ret)
		.then_ignore(just(Token::Semi))
		.map_with_span(|((name, args), ret), span| (FuncDef { name, args, ret }, span))
}

fn parse_extern_block() -> impl Parser<Token, Item, Error = Simple<Token>> {
	just(Token::Extern)
		.ignore_then(
			parse_func_def()
				.repeated()
				.delimited_by(Token::LBracket, Token::RBracket),
		)
		.map(Item::Extern)
}

type ExprParser = Recursive<'static, Token, Spanned<Expr>, Simple<Token>>;

fn expr_with_block(expr_parser: ExprParser) -> ExprParser {
	recursive(|with_block| {
		let parse_block = parse_block(expr_parser, with_block);

		let block_expr = parse_block.map(Expr::Block);

		block_expr.map_with_span(|expr, span| (expr, span))
	})
}

fn parse_block(
	expr_parser: ExprParser,
	block_parser: ExprParser,
) -> impl Parser<Token, Block, Error = Simple<Token>> + Clone {
	let semi = just(Token::Semi).repeated();

	let with_semi = expr_parser
		.clone()
		.then_ignore(semi.at_least(1))
		.map(|(expr, span)| (Stmt::Semi(expr), span));

	let no_semi = block_parser.map(|(expr, span)| (Stmt::Expr(expr), span));

	let last = expr_parser
		.map(|(expr, span)| (Stmt::Expr(expr), span))
		.or_not();

	with_semi
		.or(no_semi)
		.repeated()
		.then(last)
		.delimited_by(Token::LBracket, Token::RBracket)
		.map(|(mut stmts, last)| {
			if let Some(last) = last {
				stmts.push(last);
			}
			Block { stmts }
		})
}

fn parse_expr() -> impl Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone {
	recursive(|expr| {
		let lit = filter_map(|span, tok| match tok {
			Token::Lit(literal) => Ok(Expr::Lit(literal)),
			_ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
		});

		let ident = filter_map(|span, tok| match tok {
			Token::Ident(ident) => Ok(Expr::Ident(ident)),
			_ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
		});

		let with_block_parser = expr_with_block(expr.clone());
		let primary = with_block_parser.map(|(expr, _)| expr).or(lit).or(ident);

		let call = primary
			.then(
				expr.clone()
					.separated_by(just(Token::Comma))
					.allow_trailing()
					.delimited_by(Token::LParen, Token::RParen)
					.repeated(),
			)
			.foldl(|expr, args| Expr::Call {
				expr: Box::new(expr),
				args,
			});

		call.map_with_span(|expr, span| (expr, span))
	})
}

fn parse_func_sig() -> impl Parser<Token, Spanned<FuncSig>, Error = Simple<Token>> {
	let ident = filter_map(|span, tok| match tok {
		Token::Ident(ident) => Ok(ident),
		_ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
	});

	let args = ident
		.then_ignore(just(Token::Colon))
		.then(parse_type())
		.map_with_span(|arg, span| (arg, span))
		.separated_by(just(Token::Comma))
		.allow_trailing()
		.delimited_by(Token::LParen, Token::RParen)
		.or_not()
		.map(|args| args.unwrap_or_default());

	let ret = just(Token::ThinArrow)
		.ignore_then(parse_type())
		.or_not()
		.map(|ret| ret.unwrap_or(Type::unit()));

	just(Token::Fun)
		.ignore_then(ident)
		.then(args)
		.then(ret)
		.map_with_span(|((name, args), ret), span| {
			let out = FuncSig { name, args, ret };
			(out, span)
		})
}

fn parse_func() -> impl Parser<Token, Function, Error = Simple<Token>> {
	// let with_semi = parse_expr();

	// let no_semi = expr_with_block(with_semi.clone().boxed());

	// let body = with_semi.then_ignore(just(Token::Semi)).or(no_semi);

	parse_func_sig()
		.then_ignore(just(Token::Eq))
		.then(parse_expr())
		.map(|(sig, body)| Function { sig, body })
}
