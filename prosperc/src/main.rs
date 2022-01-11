use std::path::Path;

use anyhow::{anyhow, Result};
use chumsky::{Parser as _, Stream};
use clap::Parser;

use crate::analyze::analyze;

mod analyze;
mod ast;
mod codegen;
mod lex;
mod span;
mod token;

#[derive(Parser, Debug)]
#[clap(about, version, author)]
struct Args {
	/// Path to the file to compile
	path: String,
}

fn compile(path: &str) -> Result<()> {
	let path = Path::new(path);
	let file = std::fs::read_to_string(path)?;
	let tokens = lex::lex()
		.parse(file.as_str())
		.map_err(|errs| anyhow!("{:?}", errs))?;
	// for (token, _) in tokens.iter() {
	//     println!("{:?}", token);
	// }
	let len = file.chars().count();
	let tree = ast::parser()
		.parse(Stream::from_iter(len..len + 1, tokens.into_iter()))
		.map_err(|errs| anyhow!("{:?}", errs))?;
	// println!("{:#?}", tree);
	let comp_units = analyze(tree);
	// SAFETY: We have read the file earlier so the path must be a valid path to a file.
	let module_name = unsafe { path.file_name().unwrap_unchecked() };
	let module_name = module_name.to_string_lossy();
	codegen::compile(module_name.into(), comp_units);
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
