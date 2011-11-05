all:	Score4.class

Score4.class:	Score4.java
	# I dont know how to compile "optimizations" for Java - help?
	javac $<

test:	| all
	@echo -n 'Benchmarking imperative Java ...' ; for i in {1..10} ; do sh -c "time java Score4 o53 y43 -debug" 2>&1 | grep '^real' | sed 's,0m,,;s,s,,;' | awk '{print $$2;}' ; done | ../stats.py  | grep ^Min | sed 's,^Min,,'

clean: 
	rm -f *.class
