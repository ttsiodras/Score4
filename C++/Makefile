TARGETS=score4 score4_extreme

CXXFLAGS += -O3 -Wall -DNDEBUG -mtune=native -march=native -std=c++0x

all:	$(TARGETS)


test:	|all
	@echo -n 'C++(imperative)' ; for i in {1..10} ; do sh -c "time ./score4 o53 y43 -debug" 2>&1 | grep '^real' | sed 's,0m,,;s,s,,;' | awk '{print $$2;}' ; done | ../stats.py  | grep ^Min | sed 's,^Min,,'
	@echo -n 'C++(imperative,memoized)' ; for i in {1..10} ; do sh -c "time ./score4_extreme o53 y43 -debug" 2>&1 | grep '^real' | sed 's,0m,,;s,s,,;' | awk '{print $$2;}' ; done | ../stats.py  | grep ^Min | sed 's,^Min,,'

clean:
	@rm -rf $(TARGETS)
