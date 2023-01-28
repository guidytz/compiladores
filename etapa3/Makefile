main:
	cargo build --release
	cp target/release/etapa3 .

debug:
	cargo build
	cp target/debug/etapa3 .

clean:
	rm -f etapa4 Cargo.lock
	rm -f -r target

zip:
	tar cvzf etapa3.tgz Makefile Cargo.toml build.rs src/
