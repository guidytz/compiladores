#[cfg(feature = "lexparser")]
use cfgrammar::yacc::YaccKind;
#[cfg(feature = "lexparser-input-test")]
use cfgrammar::yacc::{YaccKind, YaccOriginalActionKind};
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
    #[cfg(feature = "lexparser-input-test")]
    CTLexerBuilder::new()
        .lrpar_config(|ctp| {
            ctp.yacckind(YaccKind::Original(YaccOriginalActionKind::NoAction))
                .grammar_in_src_dir("rawparser.y")
                .unwrap()
        })
        .lexer_in_src_dir("scanner.l")?
        .build()?;
    Ok(())
}
