default: 
	@just --list

prep:
	mkdir -p target

std: prep
	clang-13 -c examples/std.c -o target/std.o

build FILE: prep std
	cargo run --manifest-path=./prosperc/Cargo.toml -- {{FILE}} 2>/dev/null
	clang-13 `llvm-config-13 --ldflags --libs core` -o target/output target/output.o target/std.o

run FILE: (build FILE)
	@./target/output
