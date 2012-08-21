all:	Score4.class

Score4.class:	Score4.java
	# I dont know how to compile "optimizations" for Java - help?
	javac $<

include ../Makefile.common

test:	| all
	@echo -n 'Benchmarking imperative Java ...'
	@BINARY="java Score4" make -s speedtest

clean: 
	rm -f *.class
