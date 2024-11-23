.ONESHELL:
.PHONY: builds fonts watch clean

build:
	nix run . rebuild
watch:
	nix run . rebuild
	nix run . watch
clean:
	nix run . clean
fonts:
	make clean
	make build
	./scripts/opt-fonts.py
