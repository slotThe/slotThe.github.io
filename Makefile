.ONESHELL:

.PHONY: build
build:
	nix run . rebuild

.PHONY: watch
watch:
	nix run . rebuild
	nix run . watch

.PHONY: clean
clean:
	nix run . clean
