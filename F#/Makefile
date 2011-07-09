UNAME = $(shell uname)

ifeq ($(UNAME), CYGWIN_NT-5.1)
MONO:=
FSHARPCOMPILER:="/c/Program Files/Microsoft F\#/v4.0/Fsc.exe"
else
MONO:=mono
FSHARPCOMPILER:=/opt/FSharp-2.0.0.0/bin/fsc.exe
endif

TARGETS:=score4.exe score4_functional.exe

all:	$(TARGETS)

%.exe:	%.fs
	$(MONO) $(FSHARPCOMPILER) --checked- --optimize+ $^

test:	| $(TARGETS)
	sh -c "time $(MONO) ./score4_functional.exe o53 y43 -debug"
	@echo "That was F# (functional)"
	@echo "========================"
	sh -c "time $(MONO) ./score4.exe o53 y43 -debug"
	@echo "That was F# (imperative, C++ mirror)"
	@echo "===================================="

clean:
	rm -f $(TARGETS)
