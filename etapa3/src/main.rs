/*
    Grupo H
    Integrante: Guilherme Dytz dos Santos

    A implementação padrao do stdin faz com que um buffer fique alocado
    ao final da execução do programa e, com isso, o valgrind mostra que
    existe memória alocada ainda alcançável.
    Existe uma issue no repositório da linguagem que esclarece melhor o
    problema: https://github.com/rust-lang/rust/issues/80406.
    Tentei algumas formas de fazer o free manualmente, mas o buffer
    alocado aparentemente fica em uma parte privada do código da lib std,
    o que faz eu não ter acesso a ele para conseguir, de fato, liberar
    esse trecho de memória.

*/

use std::io::{self, Read, Write};

use lrlex::lrlex_mod;
#[cfg(feature = "lexparser")]
use lrpar::lrpar_mod;

// Using `lrlex_mod!` brings the lexer for `scanner.l` into scope.
lrlex_mod!("scanner.l");
// Using `lrpar_mod!` brings the lexer for `parser.y` into scope.
#[cfg(feature = "lexparser")]
lrpar_mod!("parser.y");

fn main() {
    io::stdout().flush().ok();
    let mut input = String::new();
    // let f = std::fs::read("test.txt").unwrap();
    // input = String::from_utf8(f).unwrap();

    let mut stdin = io::stdin();
    stdin
        .read_to_string(&mut input)
        .expect("Could not read from stdin");

    #[cfg(feature = "lexparser")]
    {
        let lexerdef = scanner_l::lexerdef();
        let lexer = lexerdef.lexer(&input);
        let (tree, errors) = parser_y::parse(&lexer);
        if !errors.is_empty() {
            for err in errors {
                eprintln!("{}", err.pp(&lexer, &parser_y::token_epp));
            }
            std::process::exit(1);
        }

        let tree = tree.unwrap().unwrap();
        #[cfg(feature = "debug")]
        println!("{:#?}", tree);

        tree.print(&lexer);
    }
}
