all:

test:
	@echo -n 'Benchmarking imperative Lisp (SBCL) ...'
	@sbcl --load build-and-run-SBCL.cl >log 2>/dev/null || { echo missing ; rm -f log ; exit 0 ; }
	@if [ -s log ] ; then cat log | grep -A1 Eva | grep seconds | awk '{print $$1}' | ../stats.py  | grep ^Min | sed 's,^Min,,' ; fi
	@rm -f log
	#@echo -n 'Benchmarking imperative LISP (CMUCL) ...'
	#@cmucl -batch -quiet -load build-CMUCL.cl >log 2>compiler.notes || { echo missing ; rm -f log ; exit 0 ; }
	#@cmucl -batch -quiet -load bench-CMUCL.cl >log 2>/dev/null || { echo failed ; rm -f log ; exit 0 ; }
	#@if [ -s log ] ; then cat log | grep -A1 Eva | grep seconds | awk '{print $$2}' | ../stats.py  | grep ^Min | sed 's,^Min,,' ; fi
	#@rm -f log

clean:
	rm compiler.notes *.fasl 
