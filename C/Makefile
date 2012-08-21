TARGETS=score4

CFLAGS += -O3 -Wall -DNDEBUG -mtune=native -march=native

all:	$(TARGETS)

include ../Makefile.common

test:	|all
	@echo -n 'Benchmarking imperative C ...'
	@BINARY=./score4 make -s speedtest

clean:
	@rm -rf $(TARGETS)
