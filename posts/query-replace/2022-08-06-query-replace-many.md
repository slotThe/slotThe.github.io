---
title: How to query-replace multiple matches!
tags: emacs
---

As its name suggests, Emacs's `query-replace` function (bound to `M-%`
by default) can be used to replace occurences of one string with
another.  In comparison to other tools that are used for similar
purposes—(a subset of) keyboard macros and multiple-cursors—the whole
process after entering the `from` and `to` strings is interactive all
the way through: it's very fast to step through the individual matches
and decide on the spot whether one would like to replace them or not.
Needless to say, I like `query-replace` a lot!  In true Emacs fashion,
the function also takes way too many arguments: among other things, it
can operate on the current region, backwards, or only on things
surrounded by words boundaries.

However, there is one crucial feature missing from its default
functionality: the ability to create multiple `from → to` pairs.  But
this is Emacs, after all, which means that I can just write that
`query-replace-many` function I've always wanted!

<!--more-->

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

[write a lot of LaTeX]: ../phd-workflow/2022-05-01-my-phd-workflow.html
[work]: ../../research.html

# The Solution

Thankfully, since `replace.el` sports a decent API, writing a version of
`query-replace` that accepts multiple arguments turns out to be easy
enough.  The high-level overview is this: we read in multiple queries
until an empty input is given[^1], build up a regular expression of the
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

<img class="pure-img" src="./query-replace-many.gif">

The only cosmetic imperfection of this is that, while the replacement
candidate itself is correctly updated, we see the whole regular
expression `\(?U:\|T\)` as the thing to be replaced instead of the bit
that's actually matching currently.  However, since this would seem to
require some work and one of course sees what's to be replaced by
looking at the thing at point, I can very much live with this for the
moment.

## The Code

Below is the full source code, in all of its hacky glory.  Note that you
will need to `require` the `s.el` and `dash.el` libraries for this to
work, if you haven't loaded these already (if you use any amount of
packages at all, chances are that you have).

``` emacs-lisp
(defun slot/get-queries (&optional pairs)
  "Get multiple `query-replace' pairs from the user.
PAIRS is a list of replacement pairs of the form (FROM . TO)."
  (-let* (((from to delim arg)
           (query-replace-read-args
            (s-join " "
                    (-non-nil
                     (list "Query replace many"
                           (cond ((eq current-prefix-arg '-) "backward")
                                 (current-prefix-arg         "word"))
                           (when (use-region-p) "in region"))))
            nil))                       ; no regexp-flag
          (from-to (cons (regexp-quote from)
                         (s-replace "\\" "\\\\" to))))
    ;; HACK: Since the default suggestion of replace.el will be
    ;; the last one we've entered, an empty string will give us
    ;; exactly that.  Instead of trying to fight against this,
    ;; use it in order to signal an exit.
    (if (-contains? pairs from-to)
        (list pairs delim arg)
      (slot/get-queries (push from-to pairs)))))

(defun slot/query-replace-many
    (pairs &optional delimited start end backward region-noncontiguous-p)
  "Like `query-replace', but query for several replacements.
Query for replacement pairs until the users enters an empty
string (but see `slot/get-queries').

Refer to `query-replace' and `perform-replace' for what the other
arguments actually mean."
  (interactive
   (let ((common (slot/get-queries)))
     (list (nth 0 common) (nth 1 common)
           (if (use-region-p) (region-beginning))
           (if (use-region-p) (region-end))
           (nth 2 common)
           (if (use-region-p) (region-noncontiguous-p)))))
  (perform-replace
   (concat "\\(?:" (mapconcat #'car pairs "\\|") "\\)") ; build query
   (cons (lambda (pairs _count)
           (cl-loop for (from . to) in pairs
                    when (string-match from (match-string 0))
                    return to))
         pairs)
   :query :regexp
   delimited nil nil start end backward region-noncontiguous-p))
```

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
