SCORE4PYPY:=score4-pypy

all:	${SCORE4PYPY}

test:	${SCORE4PYPY}
	@echo -n 'Benchmarking imperative Python (Pypy) ...' ; for i in {1..10} ; do bash -c "time ./${SCORE4PYPY} o53 y43 -debug" 2>&1 | grep '^real' | sed 's,0m,,;s,s,,;' | awk '{print $$2;}' ; done | ../stats.py  | grep ^Min | sed 's,^Min,,'

score4-pypy:	score4.py
	python2 /opt/pypy-pypy-2346207d9946/pypy/translator/goal/translate.py --batch --output ${SCORE4PYPY} score4.py

clean:
	rm -f ${SCORE4PYPY} *.pyc
