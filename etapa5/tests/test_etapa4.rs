#[cfg(test)]
mod test {
    use etapa5::{clear_stack, new_scope};
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
                    new_scope();

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
}
