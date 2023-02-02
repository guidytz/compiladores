#[cfg(test)]
mod test {
    use lrlex::{lrlex_mod, LexerDef};
    use lrpar::{lrpar_mod, Lexeme, NonStreamingLexer};

    lrlex_mod!("scanner.l");
    lrpar_mod!("rawparser.y");

    #[test]
    fn test_etapa2() {
        let inputs = std::fs::read_dir("./tests/etapa2/professor_entries/inputs")
            .expect("Couldn't find inputs dir");
        let outputs = std::fs::read_dir("./tests/etapa2/professor_entries/outputs")
            .expect("Couldn't find outputs dir");

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
                    let errors = rawparser_y::parse(&lexer);

                    assert_eq!(expected_output.is_empty(), errors.is_empty());
                }
                _ => panic!("Something went wrong with entry dirs"),
            }
        }
    }
}
