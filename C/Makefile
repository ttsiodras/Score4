TARGETS=score4

CFLAGS += -O3 -Wall -DNDEBUG -mtune=native -march=native

all:	$(TARGETS)

test:	|all
	sh -c "time ./score4 o53 y43 -debug"
	@echo "That was C"
	@echo "=========="

clean:
	@rm -rf $(TARGETS)
