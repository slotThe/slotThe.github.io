.ONESHELL:

.PHONY: build
build:
	stack build
	stack exec site rebuild

.PHONY: watch
watch:
	stack build
	stack exec site rebuild
	stack exec site watch
