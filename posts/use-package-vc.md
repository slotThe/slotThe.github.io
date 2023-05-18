---
title: "Use-package now has a :vc keyword"
date: 2023-05-18
tags: emacs
no-toc: true
---

Just a quick heads-up: `use-package`,
which was merged into Emacs in [November last year][emacs:use-package-merge],
now has a `:vc` keyword!

<!--more-->

The change was [merged][emacs:vc-merge] two days ago,
and supersedes—indeed, is a rewrite of—[vc-use-package],
which is now only needed for people who prefer to stick to released versions of Emacs.[^3]
In short,
the keyword enables one to install packages directly from their remote source:

``` emacs-lisp
(use-package modus-themes
  :vc (:url "https://gitlab.com/protesilaos/modus-themes"
       :branch "main"))
```

This is not dependent on git,
but should work for all version control systems that Emacs knows about;
see `vc-handled-backends`.

By default,
`:vc` installs the latest *release* of a package—the
last commit that bumped the `"Version"` tag inside of the main elisp file
(yes, really).
Installing the most recent commit instead,
which should feel more familiar to users coming from package archives like MELPA,
can be done by giving `:vc` a `:rev :newest` argument.
Additionally,
`:rev` can also be used to pin specific revisions of a package.
Other niceties, like specifying alternative lisp directories, are also included:

``` emacs-lisp
(use-package vertico
  :vc (:url "https://github.com/minad/vertico"
       :rev :newest
       :lisp-dir "extensions/"))
```

For more information on the specific syntax,
refer to `C-h v package-vc-selected-packages RET`,
and the relevant info node `(emacs)Fetching Package Sources`.

[emacs:use-package-merge]: https://git.savannah.gnu.org/cgit/emacs.git/commit/?id=4a1e9d61b57c36255752437a2668e037e79fe870
[emacs:vc-merge]: https://git.savannah.gnu.org/cgit/emacs.git/commit/?id=2ce279680bf9c1964e98e2aa48a03d6675c386fe
[vc-use-package]: https://github.com/slotThe/vc-use-package

[^3]: There is no reason for that, of course.
      Building Emacs is just a

      ``` shell
      BUILD_OPTS=$(emacs \
        --batch \
        --eval "(prin1 system-configuration-options)")

      ./autogen.sh
      echo "$BUILD_OPTS" | sed 's/^"\(.*\)"$/\1/' \
                         | xargs ./configure
      make bootstrap
      sudo make install
      ```

      away!
