#[cfg(test)]
mod test {
    use lrlex::lrlex_mod;
    use lrpar::lrpar_mod;

    lrlex_mod!("scanner.l");
    lrpar_mod!("parser.y");

    #[test]
    fn test_etapa2() {
        let inputs = std::fs::read_dir("./tests/etapa2/inputs").unwrap();
        let outputs = std::fs::read_dir("./tests/etapa2/outputs").unwrap();

        for (input, output) in inputs.into_iter().zip(outputs) {
            match (input, output) {
                (Ok(input), Ok(output)) => {
                    let input = std::fs::read_to_string(input.path()).unwrap();
                    let expected_output = std::fs::read_to_string(output.path()).unwrap();
                    let lexerdef = scanner_l::lexerdef();
                    let lexer = lexerdef.lexer(&input);
                    let (_, errors) = parser_y::parse(&lexer);

                    assert_eq!(expected_output.is_empty(), errors.is_empty());
                }
                _ => panic!("Something went wrong with entry dirs"),
            }
        }
    }
}
