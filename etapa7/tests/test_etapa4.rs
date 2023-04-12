#[cfg(test)]
mod test {
    use etapa7::{clear_stack, new_scope, semantic_aux::ScopeType};
    use lrlex::lrlex_mod;
    use lrpar::lrpar_mod;

    lrlex_mod!("scanner.l");
    lrpar_mod!("parser.y");

    #[test]
    fn test_etapa4() {
        let inputs =
            std::fs::read_dir("../test_data/etapa4/inputs").expect("Could not read input dir");

        for input in inputs {
            match input {
                Ok(input) => {
                    let input_file_name = input
                        .file_name()
                        .to_str()
                        .expect("Couldn't parse input file name")
                        .to_owned();

                    let input = std::fs::read_to_string(input.path())
                        .expect(format!("Couldn't read file {}", input_file_name).as_str());

                    let expected_output = std::fs::read_to_string(format!(
                        "../test_data/etapa4/outputs/{}",
                        input_file_name
                    ))
                    .expect("Couldn't read expected output file")
                    .parse::<u8>()
                    .expect("Could not parse integer from expected output");

                    clear_stack();
                    new_scope(ScopeType::Global);

                    let lexerdef = scanner_l::lexerdef();
                    let lexer = lexerdef.lexer(&input);
                    let (tree, errors) = parser_y::parse(&lexer);

                    if !errors.is_empty() {
                        panic!("There should not be a lexical error in these test cases.")
                    }

                    match tree.unwrap() {
                        Ok(_) => assert!(
                            expected_output == 0,
                            "Test file {} should not be correct. Expected error: {}",
                            input_file_name,
                            expected_output
                        ),
                        Err(err) => assert_eq!(
                            err.to_err_code(),
                            expected_output,
                            "Different kind of errors in file {}. Expected: {}. Got: {}. With error message: {}",
                            input_file_name,
                            expected_output,
                            err.to_err_code(),
                            err.to_string()
                        ),
                    }
                }
                _ => panic!("Something went wrong with entry dirs"),
            }
        }
    }

    #[test]
    fn test_etapa4_professor_entries() {
        let inputs = std::fs::read_dir("../test_data/etapa4/professor_entries")
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

                clear_stack();
                new_scope(ScopeType::Global);

                let lexerdef = scanner_l::lexerdef();
                let lexer = lexerdef.lexer(&input);
                let (parsed, _) = parser_y::parse(&lexer);
                let expected_code = get_expected_code(&input);

                match parsed.unwrap() {
                    Ok(_) => assert!(
                        expected_code == 0,
                        "Parsed corretcly a wrong input in file {}. Expected code: {}",
                        input_file_name,
                        expected_code
                    ),
                    Err(err) => assert_eq!(
                        expected_code,
                        err.to_err_code(),
                        "Got wrong error in file {}. Expected: {}. Got: {}. Error message: {}",
                        input_file_name,
                        expected_code,
                        err.to_err_code(),
                        err.to_string(),
                    ),
                }
            }
        }
    }

    fn get_expected_code<'a>(input: &'a str) -> u8 {
        let first = *input.split("\n").collect::<Vec<_>>().first().unwrap();

        if !first.contains("//") {
            return 0;
        }

        match &first[2..] {
            "ERR_UNDECLARED" => 10,
            "ERR_DECLARED" => 11,
            "ERR_VARIABLE" => 20,
            "ERR_ARRAY" => 21,
            "ERR_FUNCTION" => 22,
            "ERR_CHAR_TO_INT" => 31,
            "ERR_CHAR_TO_FLOAT" => 32,
            "ERR_CHAR_TO_BOOL" => 33,
            "ERR_CHAR_VECTOR" => 34,
            "ERR_X_TO_CHAR" => 35,
            _ => 100,
        }
    }
}
