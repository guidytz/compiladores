main:
	cargo build --release
	cp target/release/etapa4 .

debug:
	cargo build --features debug
	cp target/debug/etapa4 .

debug-tree:
	cargo build --features debug-tree
	cp target/debug/etapa4 .

clean:
	cargo clean
	rm -f etapa4 Cargo.lock
	rm -rf target

test:
	cargo test --features onlylex --no-default-features --test test-etapa1 -- --nocapture
	cargo test --features lexparser --test test-etapa2 -- --nocapture
	cargo test --features lexparser --no-default-features --test test-etapa3 -- --nocapture

zip:
	tar cvzf etapa4.tgz Makefile Cargo.toml build.rs src/ tests/
