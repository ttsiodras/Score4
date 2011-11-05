all:

test:
	@sbcl --load score4.cl >/dev/null
	@echo -n SBCL\ 
	@for i in {1..10} ; do sbcl --load score4.cl |grep -A1 Eva | tail -1 | awk '{print $$1}' ; done | ../stats.py  | grep ^Min | sed 's,^Min,,'
	@cmucl -batch -quiet -load build.cl >/dev/null 2>compiler.notes
	@echo -n CMUCL
	@for i in {1..10} ; do cmucl -batch -quiet -load build.cl 2>/dev/null | grep -A1 Eva | tail -1 | awk '{print $$2}' ; done | ../stats.py  | grep ^Min | sed 's,^Min,,'

clean:
	rm compiler.notes score4.fasl
