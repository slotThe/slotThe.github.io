---
title: "Announcing: latex-change-env Version 0.3"
date: 2023-02-19
tags: emacs
---

I've just released version 0.3 of `latex-change-env`, featuring some
major improvements with regard to inline maths and macro handling; this
seems as good a time as any to talk about the package in full.  I
briefly mentioned it in the post about my [research workflow], but I
figure now that the library has reached a state where I'm not ashamed of
it anymore—at least, not at the time of writing this—it may warrant its
own post.

[research workflow]: ./my-phd-workflow.html#digital-notes

<!--more-->

# Functionality

Briefly, `latex-change-env`[^1] can be seen as an extension of AUCTeX's
built-in facilities to manipulate the current environment.  Taking
functions like `LaTeX-environment` as a base, it adds extra
functionality like deleting environments, changing to and from display
maths, "remembering" and editing labels, as well as macro[^6] and inline
maths support on top of them.  There are two main "entry points":
`latex-change-env` and `latex-change-env-cycle`.

The basic functionality may be used with a `use-package` configuration
along the lines of

``` emacs-lisp
(use-package latex-change-env
  :after latex
  :commands latex-change-env
  :bind (:map LaTeX-mode-map ("C-c r" . latex-change-env))
  :custom
  (latex-change-env-math-display '("\\[" . "\\]"))
  (latex-change-env-math-inline  '("$"   . "$")))
```

Pressing the keybinding for `latex-change-env` will pop up a selection
in the minibuffer; by default, one can delete the current
environment[^2] with `k`, modify it into something else with `m`, or
switch to display maths with `d`.  This is all controlled by the
`latex-change-env-options` variable, allowing for user-defined functions
to be inserted, should that be desired.

For example, the following video showcases switching to display maths,
changing the environment into an `equation`, and deleting it completely.

<p>
  <video width="100%" controls>
    <source src="../images/latex-change-env/basic-functionality.webm"
            type="video/webm">
    Basic functionality of `latex-change-env`: changing and deleting labels.
  </video>
</p>

## Cycling through environments

The `latex-change-env-cycle` function may be used to create a list of
environments to cycle through.  For convenience, it depends on Omar
Antolín Camarena's [math-delimiters] package, so as to facilitate a
comfortable workflow with maths environments out of the box.[^3] For
example, one could define a simple maths-based toggle

``` emacs-lisp
(defun my/insert-toggle-math ()
  (interactive)
  (latex-change-env-cycle
   '(display-math equation* align* equation align)))
```

and then bind that to `$` in `latex-mode`.  This works because
`math-delimiters-insert` is called when one is not in any environment.
Quoting from the documentation of `latex-change-env-cycle` (slightly
edited):

> Cycle through the given list of environments.  The special symbol
> `display-math` denotes a display maths environment.
>
> If one is right at the end of a display or inline maths environment,
> call `math-delimiters-insert` instead of cycling through environments.
> The same is done when not inside any environment, which, for our
> definition of environment, also includes inline maths.

The following video illustrates how `my/insert-toggle-math` might be
used.

<p>
  <video width="100%" controls>
    <source src="../images/latex-change-env/cycling-maths.webm"
            type="video/webm">
    Cycling between inline and display maths, as well as several maths environments.
  </video>
</p>

Of course, cycling also works for non-maths environments, as well as
macros; below, I bound the following to a key:

``` emacs-lisp
(defun my/cycle-macros ()
  (interactive)
  (latex-change-env-cycle
   '(textbf emph textsc textit texttt)))
```

<p>
  <video width="100%" controls>
    <source src="../images/latex-change-env/cycling-macros.webm"
            type="video/webm">
    Cycling macros
  </video>
</p>

An effort was made to make macro handling feel like a first class
citizen; for example, when cycling through possible modifications,
instead of `LaTeX-environment-list-filtered` (as would be used for
environments), the macro-specific `TeX--symbol-completion-table` is used
to generate a list of possible replacements.

[math-delimiters]: https://github.com/oantolin/math-delimiters

## Labels

When changing or deleting environments, `latex-change-env` tries to
smartly handle associated labels.  In the former case, labels have a
unique prefix associated to what environment they are defined in; for
example, `theorem` environments might start their labels with `thm:`,
while a `lemma` will have a `lem:` prefix.[^5] Further, when deleting an
environment, or switching to one that does not have an associated label
prefix, the label is (i) deleted, and (ii) stored for the session, such
that it can be restored when switching back to the original environment.

While this is all well and good, renaming and deleting labels seems of
little use when the changes aren't reflected in the rest of the project.
As such, there is an optional `latex-change-env-edit-labels-in-project`
variable.  When it is customised to `t`, a label change/deletion
triggers a project-wide `query-replace-regexp`, such that the user can
decide whether referencing labels should change as well.  This utilises
Emacs's own `project.el`, so one should make sure that the LaTeX project
is version controlled—in which case the relevant ignore file is also
respected—or otherwise recognisable by the library.[^7]

<p>
  <video width="100%" controls>
    <source src="../images/latex-change-env/label-handling.webm"
            type="video/webm">
    Label handling
  </video>
</p>

# Conclusion

That's about all of the functionality that the package currently has.
It has certainly served me quite well so far—my hope is that it will be
useful to at least one other person.

On that note: contributions welcome!  For example, something that should
not be too hard to implement is the ability to store labels not just for
the session, but permanently, by serialising the internal hash-map to a
file.  I don't know how useful this would be but, given a certain
workflow, it could certainly be worthwhile!  Another path of inquiry
might be to add better macro handling.  As I said, support is currently
limited to macros that take exactly one (mandatory) argument.  However,
the response one gets from `LaTeX-what-macro` is quite general, so I
reckon it wouldn't be too hard to cook up a more general implementation.

Again, if you want to give the package a spin then you can find it on
[GitLab][gitlab:latex-change-env], [GitHub][github:latex-change-env],
and [MELPA][melpa:latex-change-env]!

[^1]: Available on [GitLab][gitlab:latex-change-env] and
      [GitHub][github:latex-change-env], as well as
      [MELPA][melpa:latex-change-env].

[^2]: In the context of this package, by "environment" I will often mean
      a proper environment, inline or display maths, or a (simple)
      macro.

[^3]: As such, be sure that you configure `math-delim​it​ers-{inline,display}`
      accordingly.  For example, it may be useful to set

      ``` emacs-lisp
        (setq math-delimiters-display
              latex-change-env-math-display)
      ```

      and so on.

[^5]: This behaviour may be changed by customising the
      `latex-change-env-labels` variable.

[^6]: At least, macros taking exactly only argument, which is what
      support is currently restricted to; contributions welcome!

[^7]: {-} If you look closely at the video, you can see that, while the
      label changes, the "Lemma" before the reference does not.  This
      would require more sophisticated regular expressions, but—reading
      `latex​-​change-env-labels` and making some assumptions—it certainly
      seems possible to do.

[gitlab:latex-change-env]: https://gitlab.com/slotThe/change-env
[github:latex-change-env]: https://github.com/slotThe/change-env
[melpa:latex-change-env]: https://www.melpa.org/#/latex-change-env
