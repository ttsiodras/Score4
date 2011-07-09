TARGETS:=score4.bin score4_functional.bin
CRUFT=$(patsubst %.bin, %.cm?, ${TARGETS}) $(patsubst %.bin, %.o, ${TARGETS})

all:	$(TARGETS)

%.bin: %.ml
	ocamlopt -unsafe -rectypes -inline 1000 -o ./$@ $<

clean:
	rm -f $(TARGETS) $(CRUFT)

test:
	sh -c "time ./score4_functional.bin o53 y43 -debug"
	@echo "That was OCaml (functional)"
	@echo "==========================="
	sh -c "time ./score4.bin o53 y43 -debug"
	@echo "That was OCaml (imperative, C++ mirror)"
	@echo "======================================="
