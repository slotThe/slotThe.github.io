.ONESHELL:
.PHONY: builds fonts watch clean

build:
	nix run . rebuild
watch:
	make build
	nix run . watch
clean:
	cabal clean
	nix run . clean
fonts:
	make clean
	make build
	./scripts/opt-fonts.py
	make build
