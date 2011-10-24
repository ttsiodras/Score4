DIRS:="F\#" Java "C\#" OCaml C++ C

all:
	@echo Use:
	@echo 
	@echo "    make play"
	@echo "        to play a graphics game of score4 (via PyGame)"
	@echo 
	@echo "    make playSimple"
	@echo "        to play a console game of score4"
	@echo 
	@echo "    make benchmark"
	@echo "        to benchmark the 4 languages (F#/C#/OCaml/C++)"
	@echo 

play:
	make -C C++ && ./interfaces/driverGUI.py

playSimple:
	make -C C++ && ./interfaces/driver.py

benchmark:
	@for f in ${DIRS} ; do make -C "$$f" ; done
	@( for f in ${DIRS} ; do make -s -C "$$f" test 2>&1 ; done )

clean:
	for f in ${DIRS} ; do make -C "$$f" clean ; done
