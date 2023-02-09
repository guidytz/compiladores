/*
    Grupo H
    Integrante: Guilherme Dytz dos Santos
*/

use std::io::{self, Write};

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
    let mut input = Vec::new();
    unsafe {
        let mut buffer = [0 as libc::c_char; 500];
        let buffer_ptr = buffer.as_mut_ptr();
        while libc::fgets(buffer_ptr, 500, libc_stdhandle::stdin())
            != libc::PT_NULL as *mut libc::c_char
        {
            input.extend_from_slice(&buffer[..libc::strlen(buffer_ptr) as usize]);
        }
    };
    let input = input.into_iter().map(|val| val as u8).collect::<Vec<_>>();
    let input = std::str::from_utf8(input.as_slice()).expect("Could not parse input as string");

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
