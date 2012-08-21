TARGETS=score4 score4_extreme

CXXFLAGS += -O3 -Wall -DNDEBUG -mtune=native -march=native -std=c++0x

all:	$(TARGETS)

include ../Makefile.common

test:	|all
	@echo -n 'Benchmarking imperative memoized C++ ...'
	@BINARY=./score4_extreme make -s speedtest
	@echo -n 'Benchmarking imperative C++ ...'
	@BINARY=./score4 make -s speedtest

clean:
	@rm -rf $(TARGETS)
