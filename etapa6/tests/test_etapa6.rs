#[cfg(test)]
mod test {
    use std::process::Command;

    use etapa6::{clear_stack, new_scope, semantic_aux::ScopeType};
    use lrlex::lrlex_mod;
    use lrpar::lrpar_mod;

    lrlex_mod!("scanner.l");
    lrpar_mod!("parser.y");

    #[test]
    fn test_etapa6() {
        let inputs =
            std::fs::read_dir("../test_data/etapa6/inputs").expect("Could not read input dir");

        for input in inputs {
            if let Ok(input) = input {
                let input_file_name = input
                    .file_name()
                    .to_str()
                    .expect("Couldn't parse input file name")
                    .to_owned();

                let input = std::fs::read_to_string(input.path())
                    .expect(format!("Couldn't read file {}", input_file_name).as_str());

                clear_stack();
                new_scope(ScopeType::Global);

                let lexerdef = scanner_l::lexerdef();
                let lexer = lexerdef.lexer(&input);
                let (tree, _) = parser_y::parse(&lexer);

                let expected = input
                    .lines()
                    .collect::<Vec<_>>()
                    .first()
                    .unwrap()
                    .replace("// r:", "")
                    .parse::<i32>()
                    .unwrap();

                match tree {
                    Some(tree) => match tree {
                        Ok(tree) => {
                            let code = tree
                                .code()
                                .into_iter()
                                .map(|inst| inst.code_str())
                                .collect::<String>();
                            std::fs::write("test.s", code).unwrap();

                            Command::new("gcc")
                                .args(["test.s", "-o", "test"])
                                .output()
                                .unwrap();

                            let out = Command::new("./test").status().unwrap().code().unwrap();

                            std::fs::remove_file("test.s").unwrap();
                            std::fs::remove_file("test").unwrap();

                            assert_eq!(expected, out, "Wrong output for file '{input_file_name}'. Expected: {expected}. Got: {out}");
                        }
                        Err(err) => panic!("Code should parse: {err}"),
                    },
                    None => panic!("Code should parse"),
                }
            }
        }
    }
}
