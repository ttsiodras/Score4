UNAME = $(shell uname)

ifeq ($(UNAME), CYGWIN_NT-5.1)
MONO:=
CSHARPCOMPILER:=csc.exe  /checked- /optimize+ /unsafe+
else
MONO:=mono
CSHARPCOMPILER:=gmcs -checked- -optimize+ -unsafe+
endif

all:	score4.exe

score4.exe:	score4.cs
	$(CSHARPCOMPILER) $^

test:	| score4.exe
	sh -c "time $(MONO) ./score4.exe o53 y43 -debug"
	@echo "That was C# (imperative, C++ mirror)"
	@echo "===================================="

clean:
	rm -f score4.exe
