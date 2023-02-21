#[cfg(test)]
mod test {
    use lrlex::lrlex_mod;
    use lrpar::lrpar_mod;

    lrlex_mod!("scanner.l");
    lrpar_mod!("parser.y");

    #[test]
    fn test_etapa2() {
        let inputs =
            std::fs::read_dir("../test_data/etapa2/inputs").expect("Could not read input dir");
        let outputs =
            std::fs::read_dir("../test_data/etapa2/outputs").expect("Could not read output dir");

        for (input, output) in inputs.into_iter().zip(outputs) {
            match (input, output) {
                (Ok(input), Ok(output)) => {
                    let input_file_name = input
                        .file_name()
                        .to_str()
                        .expect("Couldn't parse input file name")
                        .to_owned();

                    let input = std::fs::read_to_string(input.path())
                        .expect(format!("Couldn't read file {}", input_file_name).as_str());

                    let expected_output = std::fs::read_to_string(output.path())
                        .expect("Couldn't read expected output file");

                    let lexerdef = scanner_l::lexerdef();
                    let lexer = lexerdef.lexer(&input);
                    let (_, errors) = parser_y::parse(&lexer);

                    assert_eq!(expected_output.is_empty(), errors.is_empty());
                }
                _ => panic!("Something went wrong with entry dirs"),
            }
        }
    }

    #[test]
    fn test_etapa2_professor_entries() {
        let inputs = std::fs::read_dir("../test_data/etapa2/professor_entries")
            .expect("Could not read input dir");

        for input in inputs {
            if let Ok(input) = input {
                let input_file_name = input
                    .file_name()
                    .to_str()
                    .expect("Couldn't parse input file name")
                    .to_owned();

                let input = std::fs::read_to_string(input.path())
                    .expect(format!("Couldn't read file {}", input_file_name).as_str());

                let lexerdef = scanner_l::lexerdef();
                let lexer = lexerdef.lexer(&input);
                let (_, errors) = parser_y::parse(&lexer);

                assert_eq!(input.contains("INCORRECT"), !errors.is_empty());
            }
        }
    }
}
