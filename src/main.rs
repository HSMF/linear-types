use std::path::PathBuf;

use clap::Parser as _;
use linear_type::SourceFile;

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

    let analyzed = SourceFile::open_file(&app.file)
        .expect("can read source")
        .parse()
        .expect("managed to parse")
        .infer_types()
        .expect("was able to infer types");

    if app.emit_llvm_ir {
        eprintln!("{}", analyzed.codegen_to_string().unwrap())
    }

    analyzed
        .codegen_to_obj_file("./target", app.opt_level)
        .expect("could code-gen")
        .link("./src/stdlib.c", "./target/foo")
        .expect("could link")
    ;
}
