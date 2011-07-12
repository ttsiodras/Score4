all:	Score4.class

Score4.class:	Score4.java
	# I dont know how to compile "optimizations" for Java - help?
	javac $<

test:	| all
	sh -c "time java Score4 o53 y43 -debug"
	@echo "That was Java (imperative)"
	@echo "===================================="

clean: 
	rm -f *.class
