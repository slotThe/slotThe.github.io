---
title: "Announcing: vc-use-package"
date: 2022-12-22
tags: emacs
no-toc: true
---

I'd like to announce a small package I've written: [vc-use-package].  It
is a first attempt at integrating the new (as of Emacs 29)
`package-vc.el` with the now built-in use-package.  I've already talked
about how these two interact in my [last post][post:package-vc]—you can
see this package as automating things juuuust a little more.

[post:package-vc]: https://tony-zorman.com/posts/2022-11-30-package-vc-install.html
[vc-use-package]: https://github.com/slotThe/vc-use-package

<!--more-->

# Motivation

For the last post, someone gave me some [feedback][reddit:package-vc]:
couldn't we go a little further?  In particular, they wanted a new `:vc`
keyword for use-package, much like [quelpa] has done with
[quelpa-use-package].  I already gave them a small working example in a
follow-up comment, but figured this might actually interest enough
people so that turning it into a proper package could be worth it; and
here we are!

The basic premise is really this simple—we create a handler for a new
`:vc` use-package keyword.  It can be used like so:

``` emacs-lisp
(use-package math-delimiters
  :vc (:fetcher github :repo oantolin/math-delimiters))
```

One can specify most arguments that `package-vc-install` also accepts; for example:

``` emacs-lisp
(use-package math-delimiters
  :vc (:fetcher "github"
       :repo "oantolin/math-delimiters"
       :rev "master"         ; also accepts the special `:last-release'
       :backend Git))
```

Much like quelpa-use-package, there is some care needed concerning the
interaction between this package and the `use-package-always-ensure`
variable, but this should mostly be taken care of automatically.  For
more information (and manual controls), see the
[README][vc-use-package:ensure].

# Conclusion

As I've said in the corresponding Reddit post, now that both
`package-vc.el` and use-package are built-in, someone should really add
this to Emacs proper.  Alas, since I copied the idea and most of the
initial implementation from quelpa-use-package—and thus copyright
assignment is a bit iffy—it will not be me.  Still, implementing this
from scratch does not sound so hard.  If anyone feels inspired to do
exactly this, I'd be delighted!

[quelpa-use-package]: https://github.com/quelpa/quelpa-use-package
[quelpa]: https://github.com/quelpa/quelpa
[reddit:package-vc]: https://old.reddit.com/r/emacs/comments/z9i4ce/exploring_packagevcinstall_as_an_alternative_to/iygzeum/
[vc-use-package:ensure]: https://github.com/slotThe/vc-use-package#in-combination-with-use-package-always-ensure
