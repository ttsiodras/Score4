SCORE4PYPY:=score4-pypy

all:

test:
	time sh -c "/opt/pypy/pypy-c -O score4.py o53 y43 -debug"

pypy:	score4-pypy
	time sh -c "./${SCORE4PYPY} o53 y43"

score4-pypy:	score4.py
	python2 /opt/pypy-pypy-2346207d9946/pypy/translator/goal/translate.py --batch --output ${SCORE4PYPY} score4.py

clean:
	rm -f ${SCORE4PYPY}
