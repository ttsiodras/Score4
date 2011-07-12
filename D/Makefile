TARGET:=score4.bin

all:	$(TARGET)

%.bin: %.d
	dmd -O -release -inline -of$@ $<

clean:
	rm -f $(TARGET) score4.o

test:	|all
	sh -c "time ./score4.bin o53 y43 -debug"
	@echo "That was D (imperative, C++ mirror)"
	@echo "==================================="
