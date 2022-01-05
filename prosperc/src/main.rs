#![feature(result_cloned)]


use anyhow::{Context, Result};
use clap::Parser;

mod ast;
mod lex;
mod parse;
mod span;
mod token;



#[derive(Parser, Debug)]
#[clap(about, version, author)]
struct Args {
	/// Path to the file to compile
	path: String,
}

fn compile(path: &str) -> Result<()> {
	let file = std::fs::read_to_string(path)?;
	let tokens = lex::parse(path, &file);
	let items = parse::parse(tokens);
	for item in items {
		let item = item.context("Failed to parse")?;
		println!("{:#?}", item);
	}
	println!();
	Ok(())
}

fn main() {
	let args = Args::parse();
	if let Err(e) = compile(&args.path) {
		eprintln!("[ERROR]: {}", e);
		if let Some(cause) = e.source() {
			eprintln!("Caused by: {}", cause);
		}
	}
}
