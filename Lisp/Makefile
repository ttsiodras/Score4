all:

test:
	cmucl -load build.cl 2>compiler.notes
	sbcl --load score4.cl 

clean:
	rm compiler.notes score4.fasl
