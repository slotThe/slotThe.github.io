.ONESHELL:
.PHONY: builds watch clean

build:
	nix run . rebuild
	./scripts/opt-fonts.py
	nix run . rebuild

watch:
	nix run . rebuild
	nix run . watch

clean:
	cabal clean
	nix run . clean
