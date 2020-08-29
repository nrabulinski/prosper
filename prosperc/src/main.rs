use prparse::lexer::Lexer;
use prparse::Message;
use prparse::ast::AST;
use prparse::parse::Parse;
use prparse::validate::validate_ast;
use std::path::PathBuf;

fn print_msg(title: &str, msg: &Message, file: &str, input: &str, lines: &[&str]) {
    let ((start_line, start_col), (end_line, end_col)) = msg.span.line_col(&input);
    let s = end_line.to_string().len();
    println!("\n {}--> ./{}:{}:{}", " ".repeat(s), file, start_line, start_col);
    println!(" {} |", " ".repeat(s));
    for i in start_line..=end_line {
        let c = i.to_string().len();
        let line = lines[i - 1];
        let is_first = i - start_line == 0;
        let is_last = end_line - i == 0;
        println!(" {}{} | {}", " ".repeat(s - c), i, line);
        println!(" {} | {}{} {}",
            " ".repeat(s),
            " ".repeat(if is_first {
                start_col - 1
            } else { 0 }),
            "^".repeat(match (is_first, is_last) {
                (true, false) => line.len() - start_col + 1,
                (true, true) => end_col - start_col + 1,
                (false, true) => end_col,
                (false, false) => line.len()
            }),
            if is_last { format!("{}: {}\n", title, msg.message) } else { "".to_string() }
        );
    }
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    if args.len() < 2 {
        println!("Missing file path");
    } else {
        let file = std::fs::read_to_string(&args[1]).unwrap();
        let file = file.replace('\t', "    ");
        let lexer = Lexer::new(&file);
        let tokens = lexer.tokenize();
        println!("TOKENS:");
        for token in &*tokens {
            println!("\t{:?}", token);
        }
        let (warns, errs) = (tokens.warns(), tokens.errs());
        let lines: Vec<&str> = file.lines().collect();
        if warns.len() > 0 || errs.len() > 0 {
            for warn in warns {
                print_msg("warning", warn, &args[1], &file, &lines);
            }
            for err in errs {
                print_msg("error", err, &args[1], &file, &lines);
            }
        }

        let tokens = tokens.into();
        let ast = AST::parse(&tokens);
        //println!("{:#?}", &*ast);
        let ast = match ast {
            Ok((ast, _)) => {
                println!("{:?}", ast);
                match validate_ast(&ast.items) {
                    Ok(warns) => for warn in &warns {
                        print_msg("warn", warn, &args[1], &file, &lines)
                    },
                    Err(msg) => return print_msg("error", &msg, &args[1], &file, &lines)
                }
                ast
            },
            Err(msg) => return print_msg("error", &msg, &args[1], &file, &lines)
        };
        let mut path = PathBuf::from(&args[1]);
        path.set_extension("o");
        prparse::codegen::codegen(ast, &path);
    }
}
