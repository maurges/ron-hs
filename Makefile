.PHONY: test

bench:
	stack bench --ghc-options=-O2 --benchmark-arguments='--output bench.html'
