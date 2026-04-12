---
title: About This Website
---

This site is hosted via [GitHub Pages](https://pages.github.com/),
and the full source code is available on [Microsoft GitHub][ghub:site].

My static site generator of choice is [hakyll],
which leverages [pandoc] for the actual rendering.
While the bulk of the work is done by these two programs,
there are a few thin layers on top for [syntax highlighting](#syntax-highlighting) and [prerendering LaTeX](#katex-rendering).
With the bundled nix flake,
building should hopefully be deterministic enough that a simple `make watch` creates a preview server.

Most of what makes the site "unique" is already stated in the README;
however, this page gives me the chance to expand on some of the ideas,
should they not have gotten their own blog post already.

# Sidenotes

Sidenotes—as opposed to footnotes—are implemented using
a somewhat customised [Tufte CSS][github:tufte-css],
called [sidenotes.css](https://github.com/slotThe/slotThe.github.io/blob/main/css/sidenotes.css) in my case,
as well as [SideNotesHTML.hs][sidenotes-hs] from the `pandoc-sidenote` library.[^1]
While this uses absolutely no JavaScript,
unlike most pure CSS implementations
it allows arbitrary blocks to be placed inside of sidenotes.
I have a [post][site:sidenotes] about the implementation details on the Haskell-side of things.

One tricky thing to remember is that the landing page for the blog has to
[recount](https://github.com/slotThe/slotThe.github.io/blob/5a06d94a50ed348afadece5af4da607db36f1487/src/site.hs#L130)
the sidenotes, as otherwise they'd show the wrong numbers.

# Syntax highlighting

Syntax highlighting is not provided by
[Skylighting](https://hackage.haskell.org/package/skylighting),
as pandoc would normally do,
but rather by
[pygments](https://pygments.org/).
It provides better highlighting and supports a lot more languages,
so I really see no reason not to use it.
It's still regexp-based instead of being more akin to a "real" parser,
but at least I don't have to trust random C code to not pwn me when I add a new language.

::: {.include from="scripts/pygmentize.py"}
:::

The [pygments.css](https://github.com/slotThe/slotThe.github.io/blob/main/css/pygments.css)
file is an absolute mess of different special cases,
and I feel like I need to start over quite soon,
but so far it works!
I've also [written][site:pygmentise] a few more words about my transition process from skylighting to pygments, if you're interested.

# KaTeX rendering

All LaTeX output is pregenerated with [KaTeX] and directly embedded into the HTML.
This speeds up page loading, and eliminates the need for client-side JavaScript completely.
Since this is a static site without comments or any other kind of user-generated content,
it seems almost comical to still require every visitor to render the same thing,
rather than doing it once server-side.

Interfacing with KaTeX is done with the tiny
[maths.js](https://github.com/slotThe/slotThe.github.io/blob/main/scripts/maths.js)
script.
I originally got the idea from pandoc issue
[#6651](https://github.com/jgm/pandoc/issues/6651#issuecomment-1099727774),
and the JS side of things hasn't changed much, except for removing the dependency on `deno`:

::: {.include from="scripts/maths.js"}
:::

On the hakyll site, all that's needed to interface with this is [hlKaTeX][site:impl:hlkatex].
Some more implementation considerations are mentioned in a
[dedicated post](https://tony-zorman.com/posts/katex-with-hakyll.html).

# Section marks

This is a very small thing,
but I think many sites get section marks "wrong" by placing them at the *end* of the headline.
This makes them appear somewhat "ragged" to me,
even if they are hidden most of the time (and only shown on hover) anyways.
On this site, they are instead moved to the left side of the heading,
in the space between the table of contents (or sidebar) and the main content.

# BibTeX

Citations are handled by BibTeX;
I'm used to it from writing LaTeX, and it sports some neat pandoc integration.
There is a "References" section—not mentioned in the TOC—at the end of the document,
where all citations the page may have mentioned reside and are properly aligned in a table.
Clicking on any of
[@category-theory-in-context_riehl],
[@kelly82:basic], or
[@Etingof2015]
should jump you to it.

Under the hood, pandoc—by means of [citeproc](https://github.com/jgm/citeproc)—uses some convoluted XML format to describe how citations ought to look.
Not finding anything that I'm completely happy with, the site is currently using a
[hacked-on version](https://github.com/slotThe/slotThe.github.io/blob/main/bib/style.csl)
of an
[alphanumeric DIN 1505-2 style](https://www.zotero.org/styles/din-1505-2-alphanumeric),
which most reminded me of BibLaTeX's `alphabetic` style.
I've written many more words about this in a [dedicated post][site:citations].

# No JavaScript

As many of the above points already hint at,
not using any JavaScript is an explicit goal of mine,
which also means aiming for a usable site in text-based browsers such as `eww`.
Some eye candy like small-caps and sidenotes are obviously lost,
but the main content should still be comfortably viewable.

# Fonts

If you let it, this website uses lots of [custom fonts](https://github.com/slotThe/slotThe.github.io/tree/main/css/fonts).
The default serif font is
[Alegreya](https://www.huertatipografica.com/en/fonts/alegreya-ht-pro),
[my own build](https://github.com/slotThe/hopf-mono)
of [Iosevka](https://typeof.net/Iosevka/)
stands in for anything monospaced,
and for titles I use [Vollkorn](https://vollkorn-typeface.com/).
Since I'm using KaTeX for [rendering maths](#katex-rendering),
there are also quite a few LaTeX fonts loaded whenever a page needs them.
While the site of course works with system fonts as well,
there has been a somewhat conscious choice to deviate from them.
Especially code should look exactly as it appears in my editor,
which is particularly relevant when it comes to alignment<!--
-->—Unicode characters may indeed have different apparent widths!
Further, lots of system fonts do not support some typographical features,
such as small caps[^2],
that I think give this site a certain "flair".

I do, however, want to make sure that I'm not sending 2mb per font to every visitor.
The [Web Open Font Format](https://en.wikipedia.org/wiki/Web_Open_Font_Format) exists specifically for this purpose,
but even that is not enough in a lot of cases.
Fonts are quite fully featured nowadays,
containing almost every glyph under the sun,
which just blows up their size a lot.
There thankfully exist some neat tools for ~~aggressively massaging~~ ripping out unwanted code points,
and only retaining a subset of them.
I'm using the [fontTools](https://fonttools.readthedocs.io/) Python library
to only keep the glyphs that are actually used in the generated HTML pages.

::: {.include from="scripts/opt-fonts.py"}
:::

[ghub:site]: https://github.com/slotThe/slotThe.github.io
[pandoc]: https://pandoc.org/
[hakyll]: https://jaspervdj.be/hakyll/
[KaTeX]: https://katex.org/
[github:tufte-css]: https://github.com/edwardtufte/tufte-css
[pandoc:fenced-divs]: https://pandoc.org/MANUAL.html#extension-fenced_divs
[pandoc:katex]: https://github.com/jgm/pandoc/issues/6651#issuecomment-1099727774
[sidenotes-hs]: https://github.com/jez/pandoc-sidenote/blob/master/src/Text/Pandoc/SideNoteHTML.hs
[site:citations]: /posts/hakyll-and-bibtex.html
[site:impl:fonts]: https://github.com/slotThe/slotThe.github.io/tree/main/fonts
[site:impl:hlkatex]: https://github.com/slotThe/slotThe.github.io/blob/e0c723fbff7ebd21551752c2039a7cf4aef7643f/src/site.hs#L591
[site:impl:smallcaps]: https://github.com/slotThe/slotThe.github.io/blob/c0b2407ec6b7d71cde186d76d16f46e1e66cfc10/src/site.hs#L293
[site:include-files]: https://github.com/slotThe/slotThe.github.io/blob/e0c723fbff7ebd21551752c2039a7cf4aef7643f/src/site.hs#L511-L525
[site:pygmentise]: /posts/pygmentising-hakyll.html
[site:sidenotes]: /posts/block-sidenotes.html

[^1]: Here is an example!
      By the way, you might notice some links having a little `°` after them—these are links to this domain.
      I'm honestly still not sure this is useful in any way, but I quite like the eye candy.

[^2]: Instead of proper small caps, many system fonts scale down their capital letters.
      This makes them entirely too thin, and really rather ugly looking.
      If you've not blocked hosted fonts,
      compare "this <span class="small-caps">this</span> THIS"
      with
      "<span style="font-family: var(--monospace-fonts)">this</span>
      <span class="small-caps" style="font-family: var(--monospace-fonts)">this</span>
      <span style="font-family: var(--monospace-fonts)">THIS</span>",
      and notice how small-caps (the middle "this") in the former example are well-formed,
      while in the latter they are way too thin.

      I'm sorry if this is a sort of "remember that you breathe" situation,
      and you now notice whenever people use small-caps "incorrectly" in this way.
