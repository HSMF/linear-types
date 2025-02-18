use std::path::PathBuf;

use clap::Parser as _;
use codegen::Codegen;
use inkwell::context::Context;
use parse::Parser;
use tok::{Lexer, Token};

mod ast;
mod ast_with_type_info;
mod codegen;
mod infer_types;
mod parse;
mod span;
mod tok;

#[allow(dead_code)]
fn print_tok(max_width: usize, tok: Token) {
    let start = tok.span().start.col;
    let end = tok.span().end.col;
    let width = end - start;
    print!("{}", " ".repeat(start as usize - 1));
    print!("{}", "━".repeat(width as usize));
    print!("{}", ".".repeat(max_width + 1 - end as usize));

    println!(" {:?}", tok.kind());
}

#[allow(dead_code)]
fn visualize_tokens(s: &str) {
    let lex = Lexer::new(s);

    let lines: Vec<_> = s.lines().collect();

    let max_width = lines
        .iter()
        .map(|line| line.chars().count())
        .max()
        .unwrap_or(0);

    let mut eyeline = 0;
    for tok in lex {
        while eyeline < tok.span().start.line {
            println!("{}", lines[eyeline as usize]);
            eyeline += 1;
        }

        print_tok(max_width, tok);
    }
}

#[derive(clap::Parser)]
struct App {
    file: PathBuf,
    #[clap(short = 'l', long)]
    emit_llvm_ir: bool,
    #[clap(long)]
    opt_level: Option<i32>,
}

fn main() {
    let app = App::parse();
    let src = std::fs::read_to_string(app.file).expect("can read source");
    // visualize_tokens(&src);

    let mut parser = Parser::new(Lexer::new(&src));
    let prog = parser.program();
    // dbg!(&prog);

    let prog = prog.expect("managed to parse");
    // println!("{prog}");

    let prog = infer_types::infer(prog).expect("was able to infer types");

    let ctx = Context::create();
    let mut compiled = Codegen::compile(&ctx, "hello_world", &prog);

    if app.emit_llvm_ir {
        compiled.print_to_stderr();
    }

    if let Some(opt_level) = app.opt_level {
        compiled.set_opt_level(opt_level);
    }

    compiled.emit("./target/foo.o").expect("could emit");
    std::process::Command::new("clang")
        .arg("./target/foo.o")
        .arg("-o")
        .arg("target/foo")
        .output()
        .expect("could run clang");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn trailing_comma() {
        let mut parser = Parser::new(Lexer::new("type x = (a,b )"));
        let prog1 = parser.program();
        let mut parser = Parser::new(Lexer::new("type x = (a,b,)"));
        let prog2 = parser.program();
        assert_eq!(prog1, prog2);
    }
}
