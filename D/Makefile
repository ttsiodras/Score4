TARGET:=score4.bin

all:	$(TARGET)

%.bin: %.d
	dmd -O -release -debug -inline -of$@ $<

clean:
	rm -f $(TARGET) score4.o

test:	|all
	@echo -n 'D(imperative)' ; for i in {1..10} ; do sh -c "time ./score4.bin o53 y43 -debug" 2>&1 | grep '^real' | sed 's,0m,,;s,s,,;' | awk '{print $$2;}' ; done | ../stats.py  | grep ^Min | sed 's,^Min,,'
