---
title: Externalising for arXiv
date: 2026-08-09
---

In your main LaTeX file:

``` tex
\ifdefined\tikzexternalrealjob% Pass defs to externalisation subrun
  \def\UseTikzCache{}\def\MakeTikzCache{}
  % Cross-references need main.aux, but figure jobs might then read an
  % incomplete hyperref bookmarks file.
  \PassOptionsToPackage{bookmarks=false}{hyperref}
\fi

\ifdefined\UseTikzCache
  \usetikzlibrary{external}
  \tikzexternalize[prefix=tikz-cache/, figure name=tikz-]
  \tikzset{external/mode=graphics if exists}
  \ifdefined\MakeTikzCache
    \tikzset{external/mode=convert with system call}
  \fi
  \ExplSyntaxOn\makeatletter
  \let\orig@tikzcd\tikzcd
  \let\orig@endtikzcd\endtikzcd
  \RenewDocumentEnvironment{tikzcd}{O{}+b}{
    \begin{tikzpicture}[commutative~diagrams/.cd, every~diagram, #1]
      \tl_set:Nn \l_tmpa_tl { #2 }
      \tl_replace_all:Nnn \l_tmpa_tl { \& } { \pgfmatrixnextcell }
      \tl_replace_all:Nnn \l_tmpa_tl { & } { \pgfmatrixnextcell }
      \let\saved@tikzpicture\tikzpicture
      \let\saved@endtikzpicture\endtikzpicture
      \def\tikzpicture[##1]{}
      \let\endtikzpicture\relax
      \orig@tikzcd
        \l_tmpa_tl
      \orig@endtikzcd
      \let\tikzpicture\saved@tikzpicture
      \let\endtikzpicture\saved@endtikzpicture
    \end{tikzpicture}
  }{}
  \ExplSyntaxOff\makeatother
\fi
```

A makefile to use it:

``` makefile
.PHONY: arxiv pkg clean nuke
.DEFAULT_GOAL := main.pdf

%.pdf: %.tex
	latexmk -pdf -silent $<

arxiv:
	mkdir -p tikz-cache
	$(MAKE) clean
	latexmk -pdf -silent -shell-escape -usepretex='\def\MakeTikzCache{}\def\UseTikzCache{}' main.tex

pkg: arxiv
	rm -f arxiv.tar.gz
	mkdir -p tmp
	printf '\\def\\UseTikzCache{}\\pdfoutput=1\n' | cat - main.tex > tmp/main.tex
	tar czf arxiv.tar.gz --transform='s|^tmp/||' tmp/main.tex main.bbl tikz-cache/tikz-*.{pdf,dpth}

clean:
	latexmk -C main.tex
	rm -f main.auxlock

nuke: clean
	rm -rf tikz-cache arxiv.tar.gz
```

Easy.

---

I used this verbatim for [@halbig26:gabi-monad], and it worked like a charm.
