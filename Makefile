.ONESHELL:

.PHONY: build
build:
	./build.sh

.PHONY: watch
watch:
	nix build
	nix run . rebuild
	nix run . watch

.PHONY: clean
clean:
	nix run . clean
