TARGETS=score4 score4_extreme

CXXFLAGS += -O3 -Wall -DNDEBUG -mtune=native -march=native -std=c++0x

all:	$(TARGETS)


test:	|all
	sh -c "time ./score4 o53 y43 -debug"
	@echo "That was C++"
	@echo "=============="
	sh -c "time ./score4_extreme o53 y43 -debug"
	@echo "That was C++ with unordered_map cache"
	@echo "====================================="

clean:
	@rm -rf $(TARGETS)
