---
title: Free Software
---

Below is an excerpt of projects that I'm either the author of, or heavily involved with.
For a complete list see any of the popular Git forges mentioned on the [about page](./about.html).

# Contributor

## XMonad

I'm a maintainer for [XMonad], a minimal X11 window manager written in Haskell.
In particular, this encompasses the [xmonad][xmonad:repo] repository,
where the lean core of the program resides,
[xmonad-contrib][xmonad-contrib:repo]—containing lots of user extensions for all kinds of different use-cases—<!--
-->as well as some auxiliary packages like [X11][X11:repo]: Haskell bindings to Xlib.

As I strongly believe in dogfooding, I'm an avid user of course!
My configuration can be found
[here](https://gitlab.com/slotThe/dotfiles/-/tree/master/xmonad).

## KMonad

I'm a contributor to [KMonad],
a keyboard remapping daemon in the spirit of [QMK].
However,
instead of being firmware for a specific keyboard,
it is implemented in software and thus works anywhere<!--
-->—even on your laptop![^1]

My personal keyboard configuration—[colemak-dh],
with some extras to facilitate more convenient Haskell and LaTeX programming—can be found
[here](https://gitlab.com/slotThe/dotfiles/-/blob/master/kmonad/config.kbd).

## Xmobar

Since XMonad does not have a built-in status bar,
one has to use a third party program instead—[xmobar]!
As these programs have to work together quite closely,
one can't help but also be a contributor to the latter if one uses the former.

The configuration I use is available
[here](https://gitlab.com/slotThe/dotfiles/-/blob/master/xmobar/xmobarrc.hs).

# Author

## [hmenu]

A wrapper for [dmenu] in the spirit of [yeganesh].
More concretely, hmenu displays commands in order of usage
(with an optional decay for frequency sorting),
and can open programs or given files inside of your terminal or any other chosen program.

## [vmensa]

CLI application to query and filter the menus of the different canteens at TU Dresden.

## [rq]

A tiny statically typed functional language for processing JSON on the command-line.

## Haskell Libraries

+ [optparse-applicative-cmdline-util] ([Hackage](https://hackage.haskell.org/package/optparse-applicative-cmdline-util))

  Utility functions for writing command line interfaces with [optparse-applicative].
  This is used, for example, in [vmensa].

+ [html-parse-util] ([Hackage](https://hackage.haskell.org/package/html-parse-util))

  A reimplementation of utility functions from Neil Mitchell's [TagSoup],
  as well as some extra functionality,
  for Ben Gamari's [html-parse],
  as this nicely supports `Text` and `Attoparsec`.

## Emacs modes

+ [kbd-mode]

  A major more for `.kbd` files,
  serving as the Emacs integration for KMonad's configuration.

+ [arxiv-citation] ([MELPA](https://melpa.org/#/arxiv-citation))

  Generate citation data for PDF files from the arXiv.
  Additionally, download preprints to a specified directory and open them.
  Includes [elfeed] support.

+ [latex-change-env] ([MELPA](https://melpa.org/#/latex-change-env))

  Provides a way to modify LaTeX environments,
  as well as the display math mode (seeing it as an environment of sorts).
  This includes primitive label handling:
  we remember the name of labels and can rename or remember them for later.
  This means that we can restore old labels after deleting them—very convenient!

+ [vc-use-package]

  Creates a new `:vc` keyword for use-package.
  Leveraging `package-vc.el`,
  installing packages from their direct upstream repositories
  (as opposed to, say, GNU ELPA)
  becomes very convenient.
  I've written about this package [here][post:vc-use-package],
  and about `package-vc.el` [here][post:package-vc-install].
  Note that,
  as of commit [2ce27968][emacs:vc-keyword],
  this package is built into Emacs!

+ [query-replace-many]

  A tiny package that wraps `query-replace` in order to support multiple matches.
  I've written about it [here][post:query-replace-many].

+ [yank-delimiters]

  Another tiny package that (subjectively) improves the experience of
  yanking within Emacs by trimming extraneous delimiters before inserting the string into the buffer.
  I've written about this package [here][post:yanking].

+ [anki-whitespace]

  A minor mode built on top of [anki-editor] that—due to a more lightweight syntax—provides better integration into Zettelkasten-like systems.
  It's written in such a way that implementing ones own note syntax just consists of overwriting a few functions.

# Talks

+ [Git Introduction]

  An introduction to Git for people at the mathematics faculty at TU Dresden.

[Atreus]: https://tony-zorman.com/posts/atreus-review.html
[Git Introduction]: ./talks/git-introduction.html
[KMonad]: https://github.com/kmonad/kmonad
[QMK configuration]: https://github.com/slotThe/qmk_firmware/tree/keyboardio/atreus/slotThe/keyboards/keyboardio/atreus/keymaps/slotthe
[QMK]: https://qmk.fm/
[TagSoup]: https://hackage.haskell.org/package/tagsoup
[X11:repo]: https://github.com/xmonad/X11
[XMonad]: https://xmonad.org/
[anki-editor]: https://github.com/anki-editor/anki-editor
[anki-whitespace]: https://github.com/anki-editor/anki-whitespace
[arxiv-citation]: https://github.com/slotThe/arxiv-citation
[colemak-dh]: https://colemakmods.github.io/mod-dh/
[dmenu]: https://tools.suckless.org/dmenu/
[elfeed]: https://github.com/skeeto/elfeed
[emacs:vc-keyword]: https://git.savannah.gnu.org/cgit/emacs.git/commit/?id=2ce279680bf9c1964e98e2aa48a03d6675c386fe
[hmenu]: https://github.com/slotThe/hmenu
[html-parse-util]: https://github.com/slotThe/html-parse-util
[html-parse]: https://hackage.haskell.org/package/html-parse
[html:vc-use-package]: https://tony-zorman.com/posts/vc-use-package.html
[kbd-mode]: https://github.com/kmonad/kbd-mode
[latex-change-env]: https://github.com/slotThe/change-env
[optparse-applicative-cmdline-util]: https://github.com/slotThe/optparse-applicative-cmdline-util
[optparse-applicative]: https://hackage.haskell.org/package/optparse-applicative
[post:package-vc-install]: https://tony-zorman.com/posts/package-vc-install.html
[post:query-replace-many]: https://tony-zorman.com/posts/query-replace-many.html
[post:vc-use-package]: https://tony-zorman.com/posts/vc-use-package.html
[post:yanking]: https://tony-zorman.com/posts/yanking.html
[query-replace-many]: https://github.com/slotThe/query-replace-many
[rq]: https://github.com/slotThe/rq
[vc-use-package]: https://github.com/slotThe/vc-use-package
[vmensa]: https://github.com/slotThe/vmensa
[xmobar:#656]: https://codeberg.org/xmobar/xmobar/issues/656
[xmobar]: https://codeberg.org/xmobar/xmobar
[xmonad-contrib:repo]: https://github.com/xmonad/xmonad-contrib
[xmonad:repo]: https://github.com/xmonad/xmonad
[yank-delimiters]: https://github.com/slotThe/yank-delimiters
[yeganesh]: https://hackage.haskell.org/package/yeganesh

[^1]: {-} Relatedly, I also maintain a [QMK configuration] for my [Atreus] keyboard.
