main:
	cargo build --release
	cp target/release/etapa3 .

debug:
	cargo build
	cp target/debug/etapa3 .

clean:
	rm -f etapa3 Cargo.lock
	rm -rf target

test:
	cargo test --features only-lex --no-default-features --test test_etapa1 -- --nocapture
	cargo test --features lex-yacc --test test_etapa2 -- --nocapture

zip:
	tar cvzf etapa3.tgz Makefile Cargo.toml build.rs src/ tests/
