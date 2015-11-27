TARGETS=score4

all:	$(TARGETS)

include ../Makefile.common

$(TARGETS):
	go build score4.go

test:	|all
	@echo -n 'Benchmarking imperative Go ...'
	@BINARY=./score4 make -s speedtest

clean:
	@rm -rf $(TARGETS)
