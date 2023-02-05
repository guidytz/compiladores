main:
	cargo build --release
	cp target/release/etapa3 .

debug:
	cargo build
	cp target/debug/etapa3 .

clean:
	cargo clean
	rm -f etapa3 Cargo.lock
	rm -rf target

test:
	cargo test --features onlylex --no-default-features --test test-etapa1 -- --nocapture
	cargo test --features lexparser --test test-etapa2 -- --nocapture

zip:
	tar cvzf etapa3.tgz Makefile Cargo.toml build.rs src/ tests/
