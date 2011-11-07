all:

test:
	@echo -n 'Benchmarking imperative LISP (CMUCL) ...'
	@cmucl -batch -quiet -load build.cl >log 2>compiler.notes || { echo missing ; rm -f log ; exit 0 ; }
	@if [ -s log ] ; then cat log | grep -A1 Eva | grep seconds | awk '{print $$2}' | ../stats.py  | grep ^Min | sed 's,^Min,,' ; fi
	@rm -f log
	@echo -n 'Benchmarking imperative Lisp (SBCL) ...'
	@sbcl --load score4.cl >log 2>/dev/null || { echo missing ; rm -f log ; exit 0 ; }
	@if [ -s log ] ; then cat log | grep -A1 Eva | grep seconds | awk '{print $$1}' | ../stats.py  | grep ^Min | sed 's,^Min,,' ; fi
	@rm -f log

clean:
	rm compiler.notes score4.fasl
