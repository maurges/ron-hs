.PHONY: test

test:
	stack test --fast --test-arguments "--quickcheck-max-size 30"

test-release:
	stack build --test --ghc-options=-O2 --test-arguments "--quickcheck-max-size 35"

bench:
	stack bench --ghc-options=-O2 --benchmark-arguments='--output bench.html'
