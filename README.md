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
  Unlike most implementations, [Sidenote.hs][site:sidenotes-hs] supports
  arbitrary blocks to be placed inside of sidenotes.

+ In comparison to pandoc's default, better [syntax highlighting][site:pygmentise]
  via the `pygmentize` command line utility.

+ All LaTeX output is pregenerated with [mathjax-node-page] and directly
  embedded in the HTML.  This is both faster when loading the page, and
  does not require any JavaScript on the client side.

+ [Automatic smallcaps][site:impl:smallcaps] for certain abbreviations,
  like `HTML` or `GNU`.  As such, a font that actually supports this
  (instead of rescaling capital letters) is [also provided][site:impl:fonts].

+ [Pilcrows][wiki:pilcrow] when hovering over a section title, linking
  to the respective section.

[github:tufte-css]: https://github.com/edwardtufte/tufte-css
[site:impl:fonts]: https://github.com/slotThe/slotThe.github.io/tree/main/fonts
[site:impl:smallcaps]: https://github.com/slotThe/slotThe.github.io/blob/c0b2407ec6b7d71cde186d76d16f46e1e66cfc10/src/site.hs#L293
[site:pygmentise]: https://tony-zorman.com/posts/2023-01-21-pygmentising-hakyll.html
[site:sidenotes-hs]: https://github.com/slotThe/slotThe.github.io/blob/main/src/Sidenote.hs
[site:sidenotes]: https://tony-zorman.com/posts/2023-01-27-block-sidenotes.html
[wiki:pilcrow]: https://en.wikipedia.org/wiki/Pilcrow

## Build

Because building got more complicated, there is `build.sh`, which `make`
or `make build` calls.  Most notably, this script—with the help of
[mathjax-node-page]—it pre-generates all mathematics.  This also means
that one needs to clone this repository recursively, as `math-node-page`
is included as a submodule.

For speed reasons, `make watch` disregards this, so be sure to build the
website with `make` to see the final result.

[mathjax-node-page]: https://github.com/pkra/mathjax-node-page/
