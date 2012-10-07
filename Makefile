DIRS:=

GCC_EXISTS=$(shell which gcc)
ifneq ($(GCC_EXISTS),)
DIRS:=$(DIRS) C
endif

GPP_EXISTS=$(shell which g++)
ifneq ($(GPP_EXISTS),)
DIRS:=$(DIRS) C++
endif

DMD_EXISTS=$(shell which dmd)
ifneq ($(DMD_EXISTS),)
DIRS:=$(DIRS) D
endif

SBCL_EXISTS=$(shell which sbcl)
CMUCL_EXISTS=$(shell which cmucl)
ifneq ($(SBCL_EXISTS),)
ifneq ($(CMUCL_EXISTS),)
DIRS:=$(DIRS) Lisp
endif
endif

GCCGO_EXISTS=$(shell which gccgo)
ifneq ($(GCCGO_EXISTS),)
DIRS:=$(DIRS) Go
endif

JAVA_EXISTS=$(shell which java)
JAVAC_EXISTS=$(shell which javac)
ifneq ($(JAVA_EXISTS),)
ifneq ($(JAVAC_EXISTS),)
DIRS:=$(DIRS) Java
endif
endif

OCAMLOPT_EXISTS=$(shell which ocamlopt)
ifneq ($(OCAMLOPT_EXISTS),)
DIRS:=$(DIRS) OCaml
endif

# Add these for C# and F#
#DIRS:=$(DIRS) "C\#" "F\#"

# Add this for Python
#DIRS:=$(DIRS) Python

UNAME:=$(shell uname)

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
	@echo "        to benchmark implementations (C/C++/Java/OCaml/Lisp/F#/C#)"
	@echo 

play:
ifeq (${UNAME}, Darwin)
	make -C C++ && arch -i386 python ./interfaces/driverGUI.py
else
	make -C C++ && ./interfaces/driverGUI.py
endif

playSimple:
	make -C C++ && ./interfaces/driver.py

benchmark:
	@for f in ${DIRS} ; do make -C "$$f" ; done
	@echo ======================
	@echo = Running benchmarks =
	@echo ======================
	@for f in ${DIRS} ; do make -s -C "$$f" test 2>&1 ; done

clean:
	for f in ${DIRS} ; do make -C "$$f" clean ; done
