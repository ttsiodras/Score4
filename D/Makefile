TARGET:=score4.bin

all:	$(TARGET)

%.bin: %.d
	dmd -O -release -debug -inline -of$@ $<

clean:
	rm -f $(TARGET) score4.o

include ../Makefile.common

test:	|all
	@echo -n 'Benchmarking imperative D ...'
	@BINARY=./score4.bin make -s speedtest
