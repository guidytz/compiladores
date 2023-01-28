#[cfg(feature = "lex-yacc")]
use cfgrammar::yacc::YaccKind;
use lrlex::CTLexerBuilder;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    #[cfg(feature = "only-lex")]
    CTLexerBuilder::new()
        .lexer_in_src_dir("scanner.l")?
        .build()?;
    #[cfg(feature = "lex-yacc")]
    CTLexerBuilder::new()
        .lrpar_config(|ctp| {
            ctp.yacckind(YaccKind::Grmtools)
                .grammar_in_src_dir("parser.y")
                .unwrap()
        })
        .lexer_in_src_dir("scanner.l")?
        .build()?;
    Ok(())
}
