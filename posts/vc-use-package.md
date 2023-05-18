---
title: "Announcing: vc-use-package"
date: 2022-12-22
last-modified: 2023-05-18
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

*Update (2023-05-18)*: This is now built into Emacs 30!
Check [here](https://tony-zorman.com/posts/use-package-vc.html) for more details.

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

As I said above,
this is no longer only available as an external package,
but actually [built into Emacs](https://tony-zorman.com/posts/use-package-vc.html)!
By virtue of being a part of it now,
the integration with `use-package` is a lot tighter—try it out if you're using `HEAD` anyways!
If not, then don't worry: `vc-use-package` will not go anywhere for the forseeable future.

[quelpa-use-package]: https://github.com/quelpa/quelpa-use-package
[quelpa]: https://github.com/quelpa/quelpa
[reddit:package-vc]: https://old.reddit.com/r/emacs/comments/z9i4ce/exploring_packagevcinstall_as_an_alternative_to/iygzeum/
[vc-use-package:ensure]: https://github.com/slotThe/vc-use-package#in-combination-with-use-package-always-ensure
