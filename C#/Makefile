UNAME = $(shell uname | sed 's,NT-.*,NT,')

ifeq ($(UNAME), CYGWIN_NT)
MONO:=
CSHARPCOMPILER:=csc.exe  /checked- /optimize+ /unsafe+
else
MONO:=mono
CSHARPCOMPILER:=gmcs -checked- -optimize+ -unsafe+
endif

all:	score4.exe

score4.exe:	score4.cs
	$(CSHARPCOMPILER) $^

test:	| all
	@echo -n 'Benchmarking imperative C# ...' ; for i in {1..10} ; do bash -c "time $(MONO) ./score4.exe o53 y43 -debug" 2>&1 | grep '^real' | sed 's,0m,,;s,s,,;' | awk '{print $$2;}' ; done | ../stats.py  | grep ^Min | sed 's,^Min,,'

clean:
	rm -f score4.exe
