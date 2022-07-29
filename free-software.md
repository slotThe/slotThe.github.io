---
title: FLOSS
---

Free software is quite dear to my heart and so I try to contribute as
much as I can to all kinds of different projects.  List below are either
ones that I've stuck with for a while, or personal projects that are
still actively maintained.

# Contributor

## XMonad

I'm a maintainer for [XMonad], a minimal X11 window manager written in
Haskell.  In particular, this encompasses the `xmonad` repository, where
the lean core of the program resides, `xmonad-contrib`, containing lots
of user extensions for all kinds of different use-cases, as well as some
auxillary packages like `X11`—Haskell bindings to Xlib.

As I strongly believe in dogfooding, I'm an avid user of course!  My
configuration can be found
[here](https://gitlab.com/slotThe/dotfiles/-/tree/master/xmonad/.config/xmonad).

<div class="pure-g">
 <div class="pure-u-1-2">
  <img class="pure-img" src="https://github-readme-stats.vercel.app/api/pin/?username=xmonad&repo=xmonad&show_owner=true">
 </div>
 <div class="pure-u-1-2">
  <img class="pure-img" src="https://github-readme-stats.vercel.app/api/pin/?username=xmonad&repo=xmonad-extras&show_owner=true">
 </div>
</div>
<div class="pure-g">
 <div class="pure-u-1-2">
  <img class="pure-img" src="https://github-readme-stats.vercel.app/api/pin/?username=xmonad&repo=xmonad-contrib&show_owner=true">
 </div>
 <div class="pure-u-1-2">
  <img class="pure-img" src="https://github-readme-stats.vercel.app/api/pin/?username=xmonad&repo=X11&show_owner=true">
 </div>
</div>

[XMonad]: https://xmonad.org/

## Kmonad

I'm a contributor[^1] to kmonad, a keyboard remapping daemon in the
spirit of [QMK].  However, instead of being firmware for a specific
keyboard, it is implemented in software and thus works for any
keyboard—even your laptop keyboard!

<p style="text-align:center;">
 <img class="pure-img" src="https://github-readme-stats.vercel.app/api/pin/?username=kmonad&repo=kmonad&show_owner=true" width="50%">
</p>

My personal keyboard configuration—[colemak-dh], with some extras to
facilitate more convenient Haskell and LaTeX programming—can be found
[here](https://gitlab.com/slotThe/dotfiles/-/blob/master/kmonad/.config/kmonad/x220-slot-us-colemak-dh-z.kbd).

[QMK]: https://qmk.fm/
[colemak-dh]: https://colemakmods.github.io/mod-dh/
[kmonad]: https://github.com/kmonad/kmonad

## Xmobar

Since XMonad does not have a built-in status bar, one has to use a third
party bar—[xmobar] is that bar!  As these programs have to work together
quite closely, one can't help but also be a contributor to xmobar if one
uses it with xmonad.

The configuration I use is available
[here](https://gitlab.com/slotThe/dotfiles/-/blob/master/xmobar/.config/xmobarrc/src/xmobarrc.hs).

[xmobar]: https://codeberg.org/xmobar/xmobar

## Void Linux

I maintain a few packages for the [Void] GNU/Linux distribution:

``` console
$ xmypkgs soliditsallgood@mailbox.org
cgrep
ghc
ghc-bin
kmonad
pandoc
```

The fact that this includes GHC also means bumping/updating _every_
Haskell package in case of an update—lots of fun!

[Void]: https://voidlinux.org/

# Author

## [hmenu]

I'm the author of hmenu, a wrapper for [dmenu] in the spirit of
[yeganesh].  More concretely, it displays commands in order of usage
(with an optional decay for frequency sorting) and can open programs or
(given) files inside of your terminal or any chosen program.

[dmenu]: https://tools.suckless.org/dmenu/
[hmenu]: https://gitlab.com/slotThe/hmenu
[yeganesh]: https://dmwit.com/yeganesh/

## [vmensa]

CLI application to query and filter the menus of the different canteens
at TU Dresden.

[vmensa]: https://gitlab.com/slotThe/vmensa

## Emacs

I've written too many Emacs packages—and none of them popular, at that!

### [kbd-mode]

I wrote the Emacs integration for kmonad's configuration files—that is,
kbd-mode is a major more for `.kbd` files.

[kbd-mode]: https://github.com/kmonad/kbd-mode

### [arxiv-citation] ([MELPA](https://melpa.org/#/arxiv-citation))

Generate citation data for PDF files from the arXiv.  Additionally,
download preprints to a specified directory and open them.  Includes
[elfeed] support.

[elfeed]: https://github.com/skeeto/elfeed
[arxiv-citation]: https://gitlab.com/slotThe/arxiv-citation

### [latex-change-env] ([MELPA](https://melpa.org/#/latex-change-env))

Provides a way to modify LaTeX environments, as well as the display math
mode (seeing it as an environment of sorts).  This includes primitive
label handling: we remember the name of labels and can rename or
remember them for later.  This means that we can restore old labels
after deleting them—very convenient!

[latex-change-env]: https://gitlab.com/slotThe/change-env

[^1]: As well as de-facto maintainer, as the author of kmonad is
      chronically ill and can't spend much time on the project.
      However, due to other constraints, I do not have as much time to
      commit as I would in order to actually call myself a "maintainer".
