SCORE4PYPY:=score4-pypy

all:	${SCORE4PYPY}

include ../Makefile.common

test:	${SCORE4PYPY}
	@echo -n 'Benchmarking imperative Python (Pypy) ...'
	@BINARY=${SCORE4PYPY} make -s speedtest

score4-pypy:	score4.py
	python2 /opt/pypy-pypy-2346207d9946/pypy/translator/goal/translate.py --batch --output ${SCORE4PYPY} score4.py

clean:
	rm -f ${SCORE4PYPY} *.pyc
