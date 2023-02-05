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
    let mut stdin = io::stdin().lock();
    stdin
        .read_to_string(&mut input)
        .expect("Could not read from stdin");

    #[cfg(feature = "lexparser")]
    {
        let lexerdef = scanner_l::lexerdef();
        let lexer = lexerdef.lexer(&input);
        let (tree, errors) = parser_y::parse(&lexer);
        let tree = tree.unwrap().unwrap();
        tree.print_label(&lexer);
        // tree.print();
        // println!("{:#?}", tree);
        if !errors.is_empty() {
            return;
        }
    }
}
