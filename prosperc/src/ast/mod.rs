use crate::{span::Spanned, token::Literal};

mod parse;
pub use parse::parser;

#[derive(Debug, Clone)]
pub enum Item {
	Extern(Vec<Spanned<FuncDef>>),
	Function(Function),
}

#[derive(Debug, Clone)]
pub enum Type {
	Udt(String),
	Primitive(Primitive),
	Infer,
}

impl Type {
	#[inline(always)]
	fn int() -> Self {
		Type::Primitive(Primitive::Int)
	}

	#[inline(always)]
	fn unit() -> Self {
		Type::Primitive(Primitive::Unit)
	}
}

#[derive(Debug, Clone)]
pub enum Primitive {
	Int,
	Unit,
}

#[derive(Debug, Clone)]
pub struct FuncDef {
	pub name: String,
	pub args: Vec<Spanned<(Option<String>, Type)>>,
	pub ret: Type,
}

#[derive(Debug, Clone)]
pub struct FuncSig {
	pub name: String,
	pub args: Vec<Spanned<(String, Type)>>,
	pub ret: Type,
}

#[derive(Debug, Clone)]
pub struct Function {
	pub sig: Spanned<FuncSig>,
	pub body: Spanned<Expr>,
}

#[derive(Debug, Clone)]
pub enum Expr {
	Ident(String),
	Lit(Literal),
	Block(Block),
	Call {
		expr: Box<Expr>,
		args: Vec<Spanned<Expr>>,
	},
}

#[derive(Debug, Clone)]
pub struct Block {
	pub stmts: Vec<Spanned<Stmt>>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
	Semi(Expr),
	Expr(Expr),
}
