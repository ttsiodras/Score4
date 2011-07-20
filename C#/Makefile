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
	sh -c "time $(MONO) ./score4.exe o53 y43 -debug"
	@echo "That was C# (imperative, C++ mirror)"
	@echo "===================================="

clean:
	rm -f score4.exe
