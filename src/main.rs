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

fn print_tok(max_width: usize, tok: Token) {
    let start = tok.span().start.col;
    let end = tok.span().end.col;
    let width = end - start;
    print!("{}", " ".repeat(start as usize - 1));
    print!("{}", "━".repeat(width as usize));
    print!("{}", ".".repeat(max_width + 1 - end as usize));

    println!(" {:?}", tok.kind());
}

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

fn main() {
    let src = r#"
type x = (int, (int,x))
type i = int

fn f(x: int) -> int {
    return 2 * x + 3;
}

fn main() -> int {
    let x : int = 7 + 1 * f(1);
    return x;
}
    "#;
    visualize_tokens(src);

    let mut parser = Parser::new(Lexer::new(src));
    let prog = parser.program();
    dbg!(&prog);

    let prog = prog.expect("managed to parse");
    println!("{prog}");

    let prog = infer_types::infer(prog).expect("was able to infer types");

    let ctx = Context::create();
    Codegen::compile(&ctx, "hello_world", &prog);
}

fn main2() {
    let ctx = Context::create();
    let module = ctx.create_module("mod");
    let builder = ctx.create_builder();
    let i64t = ctx.i64_type();
    let fnt = i64t.fn_type(&[i64t.into(), i64t.into(), i64t.into()], false);
    let function = module.add_function("sum", fnt, None);
    let basic_block = ctx.append_basic_block(function, "entry");
    builder.position_at_end(basic_block);
    let x = function.get_nth_param(0).unwrap().into_int_value();
    let y = function.get_nth_param(1).unwrap().into_int_value();
    let z = function.get_nth_param(2).unwrap().into_int_value();

    let sum = builder.build_int_add(x, y, "").unwrap();
    let sum = builder.build_int_add(sum, z, "").unwrap();
    let sum = builder.build_int_add(sum, z, "").unwrap();

    builder.build_return(Some(&sum)).unwrap();

    module.print_to_stderr();
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
