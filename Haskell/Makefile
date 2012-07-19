TARGET:=score4.bin

all:	$(TARGET)

%.bin: %.hs
	ghc -o $@ -O2 --make $<

clean:
	rm -f $(TARGET) score4.hi score4.o

test:	|all
	bash -c "time ./$(TARGET) o53 y43 -debug"
	@echo "That was Haskell (functional,lazy)"
	@echo "=================================="
