TARGETS:=score4.bin score4_functional.bin
CRUFT=$(shell echo *.cm? *.o)

all:	$(TARGETS)

%.bin: %.ml common.ml
	ocamlopt -unsafe -rectypes -inline 1000 -o ./$@ common.ml $<

clean:
	rm -f $(TARGETS) $(CRUFT)

include ../Makefile.common

test:	|all
	@echo -n 'Benchmarking imperative OCaml ...'
	@BINARY=./score4.bin make -s speedtest
	@echo -n 'Benchmarking functional OCaml ...'
	@BINARY=./score4_functional.bin make -s speedtest
