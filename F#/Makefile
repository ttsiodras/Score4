UNAME = $(shell uname | sed 's,NT-.*,NT,')

ifeq ($(UNAME), CYGWIN_NT)
MONO:=
FSHARPCOMPILER:="/c/Program Files (x86)/Microsoft F\#/v4.0/Fsc.exe"
else
MONO:=mono
FSHARPCOMPILER:=/opt/FSharp-2.0.0.0/bin/fsc.exe
endif

TARGETS:=score4.exe score4_functional.exe

all:	$(TARGETS)

%.exe:	%.fs
	$(MONO) $(FSHARPCOMPILER) --checked- --optimize+ $^

test:	| all
	sh -c "time $(MONO) ./score4_functional.exe o53 y43 -debug"
	@echo "That was F# (functional)"
	@echo "========================"
	sh -c "time $(MONO) ./score4.exe o53 y43 -debug"
	@echo "That was F# (imperative, C++ mirror)"
	@echo "===================================="

clean:
	rm -f $(TARGETS)
