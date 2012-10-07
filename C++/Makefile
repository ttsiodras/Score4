TARGETS=score4

# OSX compiler has no unordered_map
UNAME:=$(shell uname)
ifneq (${UNAME}, Darwin)
TARGETS=$(TARGETS) score4_extreme
endif

CXXFLAGS += -O3 -Wall -DNDEBUG -mtune=native -march=native -std=c++0x

all:	$(TARGETS)

include ../Makefile.common

test:	|all
ifneq (${UNAME}, Darwin)
	@echo -n 'Benchmarking imperative memoized C++ ...'
	@BINARY=./score4_extreme make -s speedtest
endif
	@echo -n 'Benchmarking imperative C++ ...'
	@BINARY=./score4 make -s speedtest

clean:
	@rm -rf $(TARGETS)
