use crate::{span::Spanned, token::Literal};

macro_rules! def {
	($vis:vis struct $name:ident $def:tt) => {
		def! { @ $vis, $name, struct, $def }
	};
	($vis:vis enum $name:ident $def:tt) => {
		def! { @ $vis, $name, enum, $def }
	};

	(@ $vis:vis, $name:ident, $ty:tt, $def:tt) => {
		paste::paste! {
			$vis type $name = crate::span::Spanned<[<$name Def>]>;

			#[derive(Debug, Clone)]
			$vis $ty [<$name Def>] $def

			impl [<$name Def>] {
				pub fn span(self, start: crate::span::Pos, end: crate::span::Pos, path: String) -> $name {
					crate::span::Spanned {
						val: self,
						path,
						start,
						end
					}
				}
			}
		}
	};
}

#[derive(Debug, Clone)]
pub enum Item {
	Func(Box<Function>),
	Extern(Vec<FuncDef>),
}

def! {
	pub struct Ident { pub ident: String }
}

#[derive(Debug, Clone)]
pub enum Type {
	Udt(String), // User-defined type, anything that's not built-in
	Primitive(Primitive),
	Infer,
}

pub type SpanType = Spanned<Type>;

#[derive(Debug, Clone)]
pub enum Primitive {
	Int,
	Unit,
}

def! {
	pub struct FuncDef {
		pub name: Ident,
		pub args: Vec<SpanType>,
		pub ret: SpanType,
	}
}

def! {
	pub struct Function {
		pub name: Ident,
		pub args: Vec<(Ident, SpanType)>,
		pub ret: SpanType,
		pub body: Expr,
	}
}

def! {
	pub enum Expr {
		Ident(Ident),
		Block(Box<BlockExpr>),
		Lit(Literal),
		Call { expr: Box<Expr>, args: Vec<Expr> },
	}
}

def! {
	pub enum Stmt {
		Expr(Expr),
		Semi(Expr),
	}
}

def! {
	pub struct BlockExpr {
		pub stmts: Vec<Stmt>,
		pub ty: Type
	}
}
