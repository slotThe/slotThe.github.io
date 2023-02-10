.ONESHELL:

.PHONY: build
build:
	./build.sh

.PHONY: watch
watch:
	stack build
	stack exec site rebuild
	stack exec site watch

.PHONY: clean
clean:
	stack exec site clean
