all:

test:
	@echo -n 'Benchmarking imperative Lisp (SBCL) ...' 
	@sbcl --load score4.cl |grep -A1 Eva | tail -1 | awk '{print $$1}' | ../stats.py  | grep ^Min | sed 's,^Min,,'
	@echo -n 'Benchmarking imperative LISP (CMUCL) ...'
	@cmucl -batch -quiet -load build.cl 2>compiler.notes | grep -A1 Eva | tail -1 | awk '{print $$2}' | ../stats.py  | grep ^Min | sed 's,^Min,,'

clean:
	rm compiler.notes score4.fasl
