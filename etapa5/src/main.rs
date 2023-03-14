/*
    Grupo H
    Integrante: Guilherme Dytz dos Santos
*/

use std::process::ExitCode;

#[cfg(feature = "lexparser")]
use std::io::{self, Write};

#[cfg(feature = "lexparser")]
use etapa5::new_scope;

use lrlex::lrlex_mod;
#[cfg(feature = "lexparser")]
use lrpar::lrpar_mod;

// Using `lrlex_mod!` brings the lexer for `scanner.l` into scope.
lrlex_mod!("scanner.l");
// Using `lrpar_mod!` brings the lexer for `parser.y` into scope.
#[cfg(feature = "lexparser")]
lrpar_mod!("parser.y");

fn main() -> ExitCode {
    #[cfg(feature = "lexparser")]
    {
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

        let lexerdef = scanner_l::lexerdef();
        let lexer = lexerdef.lexer(&input);

        new_scope();

        let (tree, errors) = parser_y::parse(&lexer);
        if !errors.is_empty() {
            for err in errors {
                println!("{}", err.pp(&lexer, &parser_y::token_epp));
            }
            return ExitCode::from(1);
        }

        let tree = tree.unwrap();
        match tree {
            Ok(tree) => {
                #[cfg(feature = "debug-tree")]
                println!("{:#?}", tree);

                #[cfg(feature = "code")]
                for op in tree.code() {
                    op.print();
                }

                #[cfg(not(feature = "code"))]
                {
                    let str = tree.to_string(&lexer);
                    if !str.is_empty() {
                        print!("{str}");
                    }
                }

                return ExitCode::SUCCESS;
            }
            Err(err) => {
                eprintln!("{err}");
                return ExitCode::from(err.to_err_code());
            }
        }
    }

    #[cfg(feature = "onlylex")]
    ExitCode::SUCCESS
}
