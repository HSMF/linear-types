use std::{convert::Infallible, io::Write as _, path::{Path, PathBuf}, process::exit};

use codegen::Codegen;
use infer_types::TypeError;
use inkwell::context::Context;
use parse::{ParseError, Parser};
use tok::{Lexer, Token};

pub mod ast;
pub mod ast_with_type_info;
pub mod builtin;
pub mod codegen;
pub mod infer_types;
pub mod parse;
pub mod span;
pub mod tok;

fn print_tok(max_width: usize, tok: Token) {
    let start = tok.span().start.col;
    let end = tok.span().end.col;
    let width = end - start;
    print!("{}", " ".repeat(start as usize - 1));
    print!("{}", "━".repeat(width as usize));
    print!("{}", ".".repeat(max_width + 1 - end as usize));

    println!(" {:?}", tok.kind());
}

pub fn visualize_tokens(s: &str) {
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

pub struct SourceFile {
    file_name: String,
    contents: String,
}
impl SourceFile {
    pub fn open_file(path: impl AsRef<Path>) -> std::io::Result<SourceFile> {
        let path = path.as_ref();
        let contents = std::fs::read_to_string(path)?;
        Ok(SourceFile {
            contents,
            file_name: path.file_name().unwrap().to_str().unwrap().to_owned(),
        })
    }

    pub fn parse(self) -> Result<Parsed, ParseError> {
        let mut parser = Parser::new(Lexer::new(&self.contents));
        let program = parser.program()?;
        Ok(Parsed {
            source_file: self,
            program,
        })
    }

    fn object_file_name(&self) -> String {
        let no_ext = self
            .file_name
            .char_indices()
            .rev()
            .find(|(_, ch)| *ch == '.')
            .map(|(i, _)| i)
            .unwrap_or(self.file_name.len());

        let no_ext = &self.file_name[..no_ext];
        let mut res = String::with_capacity(no_ext.len());

        for ch in no_ext.chars() {
            if matches!(ch, '-' | '/' | '\\' | '.') {
                res.push('_')
            } else {
                res.push(ch)
            }
        }

        res
    }
}

pub struct Parsed {
    source_file: SourceFile,
    program: ast::Program,
}

impl Parsed {
    pub fn infer_types(self) -> Result<WithInferredTypes, TypeError> {
        let program = infer_types::infer(self.program)?;
        Ok(WithInferredTypes {
            source_file: self.source_file,
            program,
        })
    }
}

pub struct WithInferredTypes {
    source_file: SourceFile,
    program: ast_with_type_info::Program,
}

impl WithInferredTypes {
    pub fn codegen_to_string(&self) -> Result<String, Infallible> {
        let ctx = Context::create();
        let compiled = Codegen::compile(&ctx, "hello_world", &self.program);

        Ok(compiled.print_to_string())
    }

    pub fn codegen_to_obj_file(
        self,
        artifact_dir: impl AsRef<Path>,
        opt_level: Option<i32>,
    ) -> Result<ObjectFiles, String> {
        let ctx = Context::create();
        let mut compiled = Codegen::compile(&ctx, "hello_world", &self.program);

        if let Some(opt_level) = opt_level {
            compiled.set_opt_level(opt_level);
        }

        let artifact_dir = artifact_dir.as_ref();
        let obj_file = artifact_dir.join(self.source_file.object_file_name());
        compiled.emit(&obj_file)?;

        Ok(ObjectFiles {
            // source_file: self.source_file,
            // program: self.program,
            object_file: obj_file.to_owned(),
            artifact_dir: artifact_dir.to_owned(),
        })
    }
}

pub struct ObjectFiles {
    // source_file: SourceFile,
    // program: ast_with_type_info::Program,
    object_file: PathBuf,
    artifact_dir: PathBuf,
}

impl ObjectFiles {
    pub fn link(self, stdlib: impl AsRef<Path>, target: impl AsRef<Path> ) -> Result<(), ()> {

        let stdlib_obj = self.artifact_dir.join("stdlib.o");

        let output = std::process::Command::new("clang")
            .arg(stdlib.as_ref())
            .arg("-c")
            .arg("-o")
            .arg(&stdlib_obj)
            .output()
            .expect("could compile stdlib");
        assert!(output.status.success());
        let output = std::process::Command::new("clang")
            .arg(self.object_file)
            .arg(stdlib_obj)
            .arg("-o")
            .arg(target.as_ref())
            .output()
            .expect("could run clang");

        if output.status.success() {
            std::io::stderr().write_all(&output.stderr).unwrap();
            std::io::stderr().write_all(&output.stdout).unwrap();
            exit(output.status.code().unwrap_or(1))
        }

        Ok(())
    }
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
