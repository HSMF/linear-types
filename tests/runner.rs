use linear_type::SourceFile;

#[test]
fn test_compilation() {
    for test_case in std::fs::read_dir("./tests/resources/").expect("could read resources") {
        let test_case = test_case.expect("could read entry");
        SourceFile::open_file(test_case.path())
            .expect("can read source")
            .parse()
            .expect("managed to parse")
            .infer_types()
            .expect("was able to infer types")
            .codegen_to_obj_file("./target", None)
            .expect("could code-gen")
            .link("./src/stdlib.c", "./target/foo")
            .map_err(|e| (*String::from_utf8_lossy(&e)).to_owned())
            .expect("could link");
    }
}
