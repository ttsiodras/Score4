include ../Makefile.common

all:	debug

debug:
	cargo build

release:
	cargo build --release

dynamic:
	rustc  -O -C prefer-dynamic src/main.rs

test:   |release
	@echo -n 'Benchmarking functional Rust ...'
	@BINARY=./target/release/score4 make -s speedtest
