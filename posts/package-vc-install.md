---
title: Exploring package-vc-install
date: 2022-11-30
last-modified: 2023-05-18
tags: emacs
---

The Emacs 29 release branch was just cut—and it's chock full of new
features!  In this post, I want to talk about the new
`package-vc-install` function, which allows one to install packages
directly from their respective upstream source; for example, GitHub.  It
can be seen as a built-in alternative to things like quelpa or
straight.el.

<!--more-->

*Update (2023-05-18)*:
Integration into `use-package` is now available as
a [standalone package](https://tony-zorman.com/posts/vc-use-package.html),
as well as—as of Emacs 30—a
[built-in option](https://tony-zorman.com/posts/use-package-vc.html)

# The story so far

I've been using [quelpa] and [quelpa-use-package] to install packages
that are not on any popular archive straight from source.  Especially
the latter package resulted in an almost seemless integration with the
rest of my configuration; for example:

``` emacs-lisp
(use-package math-delimiters
  :quelpa (math-delimiters :fetcher github :repo "oantolin/math-delimiters"))
```

[Recently][emacs:vc-merge], Emacs added built-in capabilities for
installing a package directly from its remote repository.  Eager to
shave yet another external package from my otherwise ever growing list,
I took `package-vc.el` out for a spin: turns out, it almost perfectly
covers the use-case for which I—and perhaps a few other people—used
quelpa up until now!

The most user-facing of these new functions is `package-vc-install`,
with signature

``` emacs-lisp
(package-vc-install PACKAGE &optional NAME REV BACKEND)
```

In the simplest case, it takes a URL pointing to some online source as
its argument and installs the respective package from there, guessing
the name from the URL.  In case that doesn't work—or one wants more
control, like requiring a specific revision—there are some other
optional arguments available, see the function's documentation.

# Customising `package-vc-install`

When a package is already installed, `package-vc-install` will ask the
user to interactively confirm whether they really want to overwrite the
existing directory.  Naturally, this is not a good experience when
trying to use this in a non-interactive fashion.

There are a few ways one could go about fixing this.  One of these is
even documented in the manual: customise `package-vc-selected-packages`
and then call `package-vc-install-selected-packages`, which works much
like `package-install-selected-packages`.  However, this feels
unergonomic to me—at least considering that I want to use
`package-vc-install` as a (hopefully) drop-in replacement for
use-package's `quelpa` keyword.  Plus, I'd rather have the information
that package X is not installed from *ELPA local to the use-package
declaration of X itself.

So, let's take the easy way out and write a small wrapper:

``` emacs-lisp
(cl-defun slot/vc-install (&key (fetcher "github") repo name rev backend)
  "Install a package from a remote if it's not already installed.
This is a thin wrapper around `package-vc-install' in order to
make non-interactive usage more ergonomic.  Takes the following
named arguments:

- FETCHER the remote where to get the package (e.g., \"gitlab\").
  If omitted, this defaults to \"github\".

- REPO should be the name of the repository (e.g.,
  \"slotThe/arXiv-citation\".

- NAME, REV, and BACKEND are as in `package-vc-install' (which
  see)."
  (let* ((url (format "https://www.%s.com/%s" fetcher repo))
         (iname (when name (intern name)))
         (pac-name (or iname (intern (file-name-base repo)))))
    (unless (package-installed-p pac-name)
      (package-vc-install url iname rev backend))))
```

This function can now be used under the `init` keyword of the
use-package macro, almost without changing the shape of the declaration
from above:

``` emacs-lisp
;; Before
(use-package math-delimiters
  :quelpa (math-delimiters :fetcher github :repo "oantolin/math-delimiters"))

;; After
(use-package math-delimiters
  :init (slot/vc-install :fetcher "github" :repo "oantolin/math-delimiters")
  ;; OR (slot/vc-install :repo "oantolin/math-delimiters")
  )
```

In case you think I cherry picked the example, [here][config:quelpa->vc]
is the full commit that exchanges quelpa for `slot/vc-install`.

# That's all folks!

Admittedly, my use of quelpa was rather primitive.  I can imagine users
more heavily invested in, for example, the `straight.el` ecosystem
probably want a bit more out of their package manager than `package.el`
can give them right now, even with the added convenience of
`package-vc.el`.  However, for me—and probably at least a few people out
there—this is quite enough.  After all, for anything more there's always
[nix] :)

[config:quelpa->vc]: https://gitlab.com/slotThe/dotfiles/-/commit/6d55ac184af125a117215a1bb812ad75c5b0ab03
[emacs:vc-merge]: https://git.savannah.gnu.org/cgit/emacs.git/commit/?id=5fa2f116799b8a7c17ff6eedd6e1b1af077c116b
[nix]: https://nixos.org/
[quelpa-use-package]: https://github.com/quelpa/quelpa-use-package
[quelpa]: https://github.com/quelpa/quelpa
