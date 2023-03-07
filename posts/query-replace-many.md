---
title: Multiple Replacements with query-replace
date: 2022-08-06
last-modified: 2023-03-07
tags: emacs
---

As its name suggests, Emacs's `query-replace` function, bound to `M-%` by default, can be used to replace occurences of one string with another—and it's quite good at what it does.
However, there is one crucial feature missing from its default functionality: the ability to create multiple `from → to` pairs.
But this is Emacs, after all, which means that I can write that `query-replace-many` function I've always wanted, and even share it with others!
The [code](#the-code) is packaged as `query-replace-many`, available on [GitLab][gitlab:query-replace-many] and [GitHub][github:query-replace-many].

<!--more-->

I quite like the workflow that `query-replace` offers.  In comparison to
other tools that are used for similar purposes—keyboard macros and
multiple-cursors—the whole process after entering the `from` and `to`
strings is interactive all the way through: it's quite fast to step
through the individual matches and decide whether one would like to
replace them or not.  In true Emacs fashion, the function also takes way
too many arguments: among other things, it can operate on the current
region, backwards, or only on things surrounded by words boundaries.

# Motivation

Originally, my motivation came through [work], where I [write a lot of
LaTeX].  When polishing papers, it sometimes happens that I would like
to change or unify the notation of certain objects in the current
equation/environment/file.

When an alteration like this only encompasses a single action, like
switching `T` to `H`, a simple `query-replace` after narrowing to the
current region of interest is totally sufficient.  For others, like
changing `T` to `H` _and_ `S` to `G`, this solution, along with
multiple-cursors and other tools people usually go for, would already be
unsatisfactory—the whole region would need to be traversed twice.  Now
imagine that you want to change `T` to `U` _and_ `U` to `T`: chaos!
Save having to give some sort of temporary name to one of the objects,
which would be even slower, `query-replace` is quite useless in this
situation.  It's possible to cook up a manual solution using the
alternative `query-replace-regexp` function and capture groups, but I'm
unsure how many people know their elisp regular expressions well enough
for that to be time efficient.  I don't, and almost certainly never
will, so it seemed much easier to automate this instead!

[write a lot of LaTeX]: ./my-phd-workflow.html
[work]: ../../research.html

# The solution

Thankfully, since `replace.el` sports a decent API, writing a version of
`query-replace` that accepts multiple arguments turns out to be easy
enough.  The high-level overview is this: we read in multiple queries
until an empty input is given,[^1] build up a regular expression of the
form `"\\(?:query-1\\|query-2\\|…\\)"`, and—when it comes to
replacing—test the current thing to be replaced against all of the
queries to select the correct one.

The beauty of this is that, since it's really just a thin wrapper over
two functions from `replace.el` that do the heavy lifting, all of the
modules regular functionality, like the keybindings and history, just
work.

For example, in the following I replace `T` with `U` and, at the same
time, `U` with `T`.  The first few matches are stepped through and the
rest is just accepted wholesale.  At the bottom, you can see the default
`query-replace` interface when interacting with the query.

<img class="pure-img"
     src="../images/query-replace/query-replace-many.gif"
     alt="query-replace-many functionality showcase">

The only cosmetic imperfection of this is that, while the replacement
candidate itself is correctly updated, we see the whole regular
expression `\(?U:\|T\)` as the thing to be replaced instead of the bit
that's actually matching currently.  However, since this would seem to
require some work and one of course sees what's to be replaced by
looking at the thing at point, I can live with this for the moment.

## The code

As one might imagine, the code is actually quite straightforward—it only
consists of two functions!  The first one is a little helper, querying
the user for multiple pairs.

``` emacs-lisp
(defun query-replace-many--get-queries (&optional pairs)
  "Get multiple `query-replace' pairs from the user.
PAIRS is a list of replacement pairs of the form (FROM . TO)."
  (pcase-let* ((`(,from ,to ,delim ,arg)
                (query-replace-read-args
                 (thread-last
                   (list "Query replace many"
                         (cond ((eq current-prefix-arg '-) "backward")
                               (current-prefix-arg         "word"))
                         (when (use-region-p) "in region"))
                   (seq-keep #'identity)
                   ((lambda (seq) (mapconcat #'identity seq " "))))
                 nil))                  ; no regexp-flag
               (from-to
                (cons (regexp-quote from)
                      (replace-regexp-in-string "\\\\" "\\\\" to t t))))
    ;; HACK: Since the default suggestion of replace.el will be the last
    ;; one we've entered, an empty string will give us exactly that.
    ;; Instead of trying to fight against this, use it in order to
    ;; signal an exit.
    (if (member from-to pairs)
        (list pairs delim arg)
      (query-replace-many--get-queries (push from-to pairs)))))
```

The actual `query-replace-many` function now just reads some pairs from
the user by virtue of the above function, and then calls
`perform-replace` with an appropriately generated regular expression.

``` emacs-lisp
(defun query-replace-many
    (pairs &optional delimited start end backward region-noncontiguous-p)
  "Like `query-replace', but query for several replacements.
Query for replacement PAIRS until the users enters an empty
string (but see `query-replace-many--get-queries').

The optional arguments DELIMITED, START, END, BACKWARD, and
REGION-NONCONTIGUOUS-P are as in `query-replace' and
`perform-replace', which see."
  (interactive
   (let ((common (query-replace-many--get-queries)))
     (list (nth 0 common)     (nth 1 common)
           (if (use-region-p) (region-beginning))
           (if (use-region-p) (region-end))
           (nth 2 common)     (if (use-region-p)
                                (region-noncontiguous-p)))))
  (perform-replace
   (concat "\\(?:" (mapconcat #'car pairs "\\|") "\\)") ; build query
   (cons (lambda (pairs _count)
           (cl-loop for (from . to) in pairs
                    when (string-match from (match-string 0))
                    return to))
         pairs)
   :query :regexp delimited nil nil start end backward
   region-noncontiguous-p))
```

And that's it!  As an aside, calling `query-replace-many` also works
from lisp; `(query-replace-many '(("1" . "2") ("2" . "1")))` has exactly
the effect one would imagine it to have.  As I said, everything is
conveniently packaged up on [GitLab][gitlab:query-replace-many] and
[GitHub][github:query-replace-many]—get it while it's hot!

[^1]: This isn't _quite_ what's actually done, but it's the right mental
      model to have (since this is how the function behaves).  The gory
      details are that we use the fact that `replace.el`'s default
      suggestion is always the last query that was entered by the user.
      What happens on an empty input is quite deep in the bowels of
      `query-replace-read-from`.  Since replacing these massive internal
      functions sounds like a real pain, leaning on that functionality
      suddenly appears much more reasonable.  Thus, when we get back a
      query that has already been entered in one way or another, we bail
      out.

[github:query-replace-many]: https://github.com/slotThe/query-replace-many
[gitlab:query-replace-many]: https://gitlab.com/slotThe/query-replace-many
