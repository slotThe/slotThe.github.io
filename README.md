# Tony's Website

Odds and ends—hopefully at least some interesting blog posts!

The base setup is based on [duplone.github.io], see [BSD-3](/BSD-3.txt).
Further—less direct—inspirations include [gwern.net][gwern.net] and
[Practical Typography][practical-typography].

[duplone.github.io]: https://github.com/duplode/duplode.github.io/
[gwern.net]: https://gwern.net/
[practical-typography]: https://practicaltypography.com/

## Notable Features

For various definitions of "notable".

+ [Sidenotes][site:sidenotes] with the help of [Tufte CSS][github:tufte-css].
  Unlike most implementations, [SideNotesHTML.hs][sidenotes-hs],
  which is now part of the `pandoc-sidenote` library,
  allows arbitrary blocks to be placed inside of sidenotes.

+ In comparison to pandoc's default, better [syntax highlighting][site:pygmentise]
  via the `pygmentize` command line utility.

+ All LaTeX output is pregenerated with [KaTeX] and directly embedded in
  the HTML. This is both faster when loading the page, and does not
  require any JavaScript on the client side.

  Interfacing with KaTeX is done with [maths.js](./scripts/maths.js),
  a tiny TS script originally from [here][pandoc:katex],
  which starts up a server that one can talk to.
  See the relevant [hlKaTeX][site:impl:hlkatex] function for more information.

+ [Automatic smallcaps][site:impl:smallcaps] for certain abbreviations,
  like `HTML` or `GNU`.  As such, a font that actually supports this
  (instead of rescaling capital letters) is [also provided][site:impl:fonts].

+ Section marks when hovering over a heading, linking to the respective section.

+ [Citation handling][site:citations] with BibTeX.

+ Straightforward [file inclusions][site:include-files] with pandoc's [fenced divs][pandoc:fenced-divs].

+ Fully featured fonts that are not 2mb in size—see
  [here](https://tony-zorman.com/site.html#fonts) for some prose,
  as well as [here](./scripts/opt-fonts.py) for the relevant script.

[KaTeX]: https://katex.org/
[github:tufte-css]: https://github.com/edwardtufte/tufte-css
[pandoc:fenced-divs]: https://pandoc.org/MANUAL.html#extension-fenced_divs
[pandoc:katex]: https://github.com/jgm/pandoc/issues/6651#issuecomment-1099727774
[sidenotes-hs]: https://github.com/jez/pandoc-sidenote/blob/master/src/Text/Pandoc/SideNoteHTML.hs
[site:citations]: https://tony-zorman.com/posts/hakyll-and-bibtex.html
[site:impl:fonts]: https://github.com/slotThe/slotThe.github.io/tree/main/fonts
[site:impl:hlkatex]: https://github.com/slotThe/slotThe.github.io/blob/main/src/site.hs#L530
[site:impl:smallcaps]: https://github.com/slotThe/slotThe.github.io/blob/c0b2407ec6b7d71cde186d76d16f46e1e66cfc10/src/site.hs#L293
[site:include-files]: https://github.com/slotThe/slotThe.github.io/blob/e0c723fbff7ebd21551752c2039a7cf4aef7643f/src/site.hs#L511-L525
[site:pygmentise]: https://tony-zorman.com/posts/pygmentising-hakyll.html
[site:sidenotes]: https://tony-zorman.com/posts/block-sidenotes.html

## Build

The tiny [Makefile](./Makefile) should be reasonably self-explanatory.
The TL;DR is to use `make` to build the website once,
or `make watch` to create an updating preview on `localhost:8000`.
