UNAME = $(shell uname | sed 's,NT-.*,NT,')

ifeq ($(UNAME), CYGWIN_NT)
MONO:=
CSHARPCOMPILER:=csc.exe  /checked- /optimize+ /unsafe+
else
MONO:=mono
CSHARPCOMPILER:=mcs -checked- -optimize+ -unsafe+
endif

all:	score4.exe

score4.exe:	score4.cs
	$(CSHARPCOMPILER) $^

include ../Makefile.common

test:	| all
	@echo -n 'Benchmarking imperative C# ...'
	@BINARY="$(MONO) ./score4.exe" make -s speedtest

clean:
	rm -f score4.exe
