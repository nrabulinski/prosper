use crate::{ast::*, span::Spanned};

pub fn analyze(items: Vec<Spanned<Item>>) -> Vec<Item> {
	// let mut res = Vec::new(); //with_capacity(items.len());
	let mut curr = Vec::with_capacity(items.len());
	for (item, _) in items {
		curr.push(item);
	}
	curr
	// res.push((name, curr));
	// res
}

struct Context {}

trait Analyze {
	fn analyze(self, ctx: Context) -> Self;
}

impl<T: Analyze> Analyze for Spanned<T> {
	fn analyze(self, ctx: Context) -> Self {
		(self.0.analyze(ctx), self.1)
	}
}
