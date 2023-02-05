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
        let tree = tree.unwrap().unwrap();

        #[cfg(feature = "debug")]
        println!("{:#?}", tree);
        if !errors.is_empty() {
            return;
        }

        tree.print();
        tree.print_label(&lexer);
    }
}
