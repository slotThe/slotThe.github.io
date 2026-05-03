.ONESHELL:
.PHONY: builds watch clean tidy

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

check: tidy
	@miss=$$(grep -L -- 'og-description:' posts/*.md); \
	if [ -n "$$miss" ]; then echo -e "Missing desc:\n$$miss"; exit 1; fi

tidy: # Courtesy of Susam (https://codeberg.org/susam/susam.net/src/branch/main/Makefile)
	find docs -name "*.html" | \
        grep -v "docs/posts/hakyll-and-bibtex.html" | \
        grep -v "docs/posts/weighted-colimits.html" | \
        grep -v "docs/posts/block-sidenotes.html" | \
        grep -v "docs/wander/index.html" | \
        grep -v "docs/hsha.html" | \
        grep -v "docs/impressum.html" | \
        grep -v "docs/mackey-functors.html" | \
	while read -r p; do \
	  echo Tidying "$$p"; \
	  sed 's/<p><\/p>//' "$$p" > /tmp/tmp.html; \
          sed 's/><\/label>/>a<\/label>/g' -i /tmp/tmp.html; \
          sed 's/><\/span>/>a<\/span>/g' -i /tmp/tmp.html; \
          sed 's/<ol type="1">/<ol>/g' -i /tmp/tmp.html; \
	  tidy -q -e --warn-proprietary-attributes no /tmp/tmp.html || exit 1; \
	done
	@echo Done; echo
