.ONESHELL:

compile:
	stack build
	stack exec site rebuild
	stack exec site watch
