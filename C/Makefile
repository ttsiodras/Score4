TARGETS=score4

CFLAGS += -O3 -Wall -DNDEBUG -mtune=native -march=native

all:	$(TARGETS)

test:	|all
	@echo -n 'Benchmarking imperative C ...' ; for i in {1..10} ; do bash -c "time ./score4 o53 y43 -debug" 2>&1 | grep '^real' | sed 's,0m,,;s,s,,;' | awk '{print $$2;}' ; done | ../stats.py  | grep ^Min | sed 's,^Min,,'

clean:
	@rm -rf $(TARGETS)
