.ONESHELL:

.PHONY: build
build:
	./build.sh

.PHONY: watch
watch:
	cabal build
	cabal exec site rebuild
	cabal exec site watch

.PHONY: clean
clean:
	cabal exec site clean
