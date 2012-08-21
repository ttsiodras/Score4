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

include ../Makefile.common

test:	| all
	@echo -n 'Benchmarking imperative F# ...'
	@BINARY="$(MONO) ./score4.exe" make -s speedtest
	@echo -n 'Benchmarking functional F# ...'
	@BINARY="$(MONO) ./score4_functional.exe" make -s speedtest

clean:
	rm -f $(TARGETS)
