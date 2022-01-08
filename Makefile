.PHONY: test

test:
	stack test --fast --test-arguments "--quickcheck-max-size 30"
