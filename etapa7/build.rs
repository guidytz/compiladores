#[cfg(feature = "lexparser")]
use cfgrammar::yacc::YaccKind;
use lrlex::CTLexerBuilder;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    #[cfg(feature = "lexparser")]
    CTLexerBuilder::new()
        .lrpar_config(|ctp| {
            ctp.yacckind(YaccKind::Grmtools)
                .grammar_in_src_dir("parser.y")
                .unwrap()
        })
        .lexer_in_src_dir("scanner.l")?
        .build()?;
    #[cfg(feature = "onlylex")]
    CTLexerBuilder::new()
        .lexer_in_src_dir("scanner.l")?
        .build()?;
    Ok(())
}
