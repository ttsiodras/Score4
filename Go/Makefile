TARGETS=score4

all:	$(TARGETS)

include ../Makefile.common

$(TARGETS):
	#go build score4.go
	gccgo -g -O3 -o score4 score4.go

test:	|all
	@echo -n 'Benchmarking imperative Go ...'
	@BINARY=./score4 make -s speedtest

clean:
	@rm -rf $(TARGETS)
