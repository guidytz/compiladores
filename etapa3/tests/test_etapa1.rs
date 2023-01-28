#[cfg(test)]
mod test {
    use std::process;

    use lrlex::{lrlex_mod, LexerDef};
    use lrpar::{Lexeme, Lexer, NonStreamingLexer};

    lrlex_mod!("scanner.l");

    #[test]
    fn test_etapa1() {
        let inputs = std::fs::read_dir("./tests/etapa1/inputs").unwrap();
        let outputs = std::fs::read_dir("./tests/etapa1/outputs").unwrap();

        for (input, output) in inputs.into_iter().zip(outputs) {
            match (input, output) {
                (Ok(input), Ok(output)) => {
                    let input = std::fs::read_to_string(input.path()).unwrap();
                    let expected_output = std::fs::read_to_string(output.path()).unwrap();
                    let lexerdef = scanner_l::lexerdef();
                    let lexer = lexerdef.lexer(&input);
                    let mut lexer_output = String::new();
                    for token in lexer.iter() {
                        match token {
                            Ok(l) => {
                                let ((line, _), _) = lexer.line_col(l.span());
                                let token = match &**lexerdef
                                    .get_rule_by_id(l.tok_id())
                                    .name
                                    .as_ref()
                                    .unwrap()
                                {
                                    "," | ";" | ":" | "(" | ")" | "{" | "}" | "+" | "-" | "*"
                                    | "/" | "%" | "^" | "<" | ">" | "=" | "!" | "[" | "]" => {
                                        "TK_ESPECIAL"
                                    }
                                    token => token,
                                };
                                lexer_output += &*format!(
                                    "{} {} [{}]\n",
                                    line,
                                    token,
                                    &input[l.span().start()..l.span().end()]
                                );
                                if token == "TK_ERRO" {
                                    break;
                                }
                            }
                            Err(e) => {
                                println!("{:?}", e);
                                process::exit(1);
                            }
                        }
                    }
                    assert_eq!(expected_output, lexer_output);
                }
                _ => panic!("Something went wrong with entry dirs"),
            }
        }
    }
}