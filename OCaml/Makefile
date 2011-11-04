TARGETS:=score4.bin score4_functional.bin
CRUFT=$(shell echo *.cm? *.o)

all:	$(TARGETS)

%.bin: %.ml
	ocamlopt -unsafe -rectypes -inline 1000 -o ./$@ common.ml $<

clean:
	rm -f $(TARGETS) $(CRUFT)

test:	|all
	@echo -n 'OCaml(functional)' ; for i in {1..10} ; do sh -c "time ./score4_functional.bin o53 y43 -debug" 2>&1 | grep '^real' | sed 's,0m,,;s,s,,;' | awk '{print $$2;}' ; done | ../stats.py  | grep ^Min | sed 's,^Min,,'
	@echo -n 'OCaml(imperative)' ; for i in {1..10} ; do sh -c "time ./score4.bin o53 y43 -debug" 2>&1 | grep '^real' | sed 's,0m,,;s,s,,;' | awk '{print $$2;}' ; done | ../stats.py  | grep ^Min | sed 's,^Min,,'
