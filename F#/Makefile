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
	@echo -n 'F#(functional)' ; for i in {1..10} ; do sh -c "time $(MONO) ./score4_functional.exe o53 y43 -debug" 2>&1 | grep '^real' | sed 's,0m,,;s,s,,;' | awk '{print $$2;}' ; done | ../stats.py  | grep ^Min | sed 's,^Min,,'
	@echo -n 'F#(imperative)' ; for i in {1..10} ; do sh -c "time $(MONO) ./score4.exe o53 y43 -debug" 2>&1 | grep '^real' | sed 's,0m,,;s,s,,;' | awk '{print $$2;}' ; done | ../stats.py  | grep ^Min | sed 's,^Min,,'

clean:
	rm -f $(TARGETS)
