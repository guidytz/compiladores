main:
	cargo build --release
	cp target/release/etapa6 .

debug:
	cargo build --features debug
	cp target/debug/etapa6 .

debug-tree:
	cargo build --features debug-tree
	cp target/debug/etapa6 .

clean:
	cargo clean
	rm -f etapa6 Cargo.lock
	rm -rf target

test: # only works with test data dir
	cargo test --features onlylex 							--no-default-features --test test-etapa1 -- --nocapture
	cargo test --features lexparser 						--no-default-features --test test-etapa2 -- --nocapture
	cargo test --features lexparser 						--no-default-features --test test-etapa3 -- --nocapture
	cargo test --features lexparser --features semantics  	--no-default-features --test test-etapa4 -- --nocapture
	cargo test --features lexparser --features semantics --features code  		  --test test-etapa6 -- --nocapture

zip:
	tar cvzf etapa6.tgz Makefile Cargo.toml build.rs src/ tests/