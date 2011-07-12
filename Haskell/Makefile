TARGET:=score4.bin

all:	$(TARGET)

%.bin: %.hs
	ghc -O -o $@ $<

clean:
	rm -f $(TARGET) score4.hi score4.o

test:	|all
	sh -c "time ./score4.bin o53 y43 -debug"
	@echo "That was Haskell (functional,lazy)"
	@echo "=================================="
