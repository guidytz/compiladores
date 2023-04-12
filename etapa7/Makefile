main:
	cargo build --release
	cp target/release/etapa7 .

debug:
	cargo build --features debug
	cp target/debug/etapa7 .

debug-tree:
	cargo build --features debug-tree
	cp target/debug/etapa7 .

clean:
	cargo clean
	rm -f etapa7 Cargo.lock
	rm -rf target

test: # only works with test data dir
	cargo test --features onlylex 							--no-default-features --test test-etapa1 -- --nocapture
	cargo test --features lexparser 						--no-default-features --test test-etapa2 -- --nocapture
	cargo test --features lexparser 						--no-default-features --test test-etapa3 -- --nocapture
	cargo test --features lexparser --features semantics  	--no-default-features --test test-etapa4 -- --nocapture
	cargo test --features lexparser --features semantics --features code  		  --test test-etapa5 -- --nocapture

zip:
	tar cvzf etapa7.tgz Makefile Cargo.toml build.rs src/ tests/

ex1: main
	./etapa7 < examples/ex1 > ex1.dot
	dot -Tpng ex1.dot -o ex1.png

ex2: main
	./etapa7 < examples/ex2 > ex2.dot
	dot -Tpng ex2.dot -o ex2.png