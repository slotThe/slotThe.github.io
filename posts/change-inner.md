---
title: Change the insides of an S-expression in Emacs
date: 2023-08-26
tags: emacs
---

I have to make a confession:
I have an [evil][ghub:evil-mode] past—literally.
Having switched to vanilla Emacs keybindings a while ago,
one thing that I genuinely miss from *that time* are the `ci(` and `ca(` motions,
killing everything in or around the closest encompassing `()`-environment.
Luckily, the [change-inner][ghub:change-inner] package provides exactly these commands for Emacs proper.
Unluckily, there are some issues regarding whitespace handling—let's try to fix that.

<!--more-->

# How it all started

After happily using change-inner for a few days,
one of the first problems I ran into
was the package's flakiness with respect to whitespace.
This is elucidated in, for example,
[this issue][ghub:change-inner#5]:

> When using change-inner with rust-mode, the following code (with `|` as the cursor):
>
> ```rust
> let issue_list_url = Url::parse(|
>     "https://github.com/rust-lang/rust/issues?labels=E-easy&state=open"
>         ).unwrap();
> ```
>
> calling `M-x change-inner (` gives:
>
> ```rust
> let issue_list_url = Url::parse|.unwrap();
> ```
>
> whereas I would expect:
>
> ```rust
> let issue_list_url = Url::parse(|).unwrap();
> ```
>
> It looks like it's related to newlines. There's a similar issue in JS:
>
> ```javascript
> // works here
> var foo = bar(|"baz");
>
> // error: Couldn't find expansion
> var foo = bar(|
>     "baz");
> ```

Change-inner as a package builds upon another excellent one from the same author:
[expand-region][ghub:expand-region],
an "Emacs extension to increase selected region by semantic units."
Essentially, change-inner just expands the region
until it hits something that it's happy with.
As such, the problem eluded to above is
with the respective expand-region functions that are called;
specifically, `er/mark-inside-pairs`,
which is defined like so:

``` emacs-lisp
(defun er/mark-inside-pairs ()
  "Mark inside pairs (as defined by the mode), not including the pairs."
  (interactive)
  (when (er--point-inside-pairs-p)
    (goto-char (nth 1 (syntax-ppss)))
    (set-mark (save-excursion
                (forward-char 1)
                (skip-chars-forward er--space-str) ; ← HERE
                (point)))
    (forward-list)
    (backward-char)
    (skip-chars-backward er--space-str)            ; ← HERE
    (exchange-point-and-mark)))
```

Notice the invocations of `(skip-chars-forward er--space-str)`;
if we start with[^2]

``` javascript
var foo = bar(|
    "baz");
```

and run `M-x er/mark-inside-pairs RET`,
then the marked area will actually just be `"baz"`,
instead of everything inside of the parentheses.

Mystery solved, right?
Maybe, but having to redefine that function for this package alone
feels wrong to me.
This got me looking into the internals of change-inner,[^9]
in order to see where the problem *actually* lies.

# Inside change-inner

Taking a closer look at `change-inner*`<!--
-->—the internal function doing the actual work—<!--
-->reveals the following.
After some initial book keeping,
the area surrounding the point is expanded,
looking for the innermost expression matching the parameters:[^5]

``` emacs-lisp
(er--expand-region-1)
(er--expand-region-1)                      ; sic!
(while (and (not (= (point) (point-min)))
            (not (looking-at q-char)))
  (er--expand-region-1))
```

Crucially,
the area is expanded twice *completely unconditionally*.
This stops only
once we've reached the bounds of either the buffer,
or the expression we are interested in.

The idea is to start in a situation like this

``` emacs-lisp
'( "one" "t|wo" "three" "four" )
```

and expand until we encompass the whole list[^10]

``` emacs-lisp
'( "one" "t|wo" "three" "four" )
;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

noting that the previous step was

``` emacs-lisp
'( "one" "t|wo" "three" "four" )
;  ^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

which equates to the "inner" part of the list.
Then, one can call
`er/contract-region`,
which relies on an expansion history,
in order to only kill this inner part.

Why expand twice unconditionally?
Because in a situation like

``` emacs-lisp
'|( "one" "two" "three" "four" )
```

The expansion would immediately encompass the whole list,[^11]
and its innards wouldn't be available to expand-region's contraction history.
The "trick" is to actually expand _further_ than necessary;
looping through the `while` above until one inevitably hits `(point-min)` and stops expanding.
This triggers yet another bit of code that then recurses with prefilled arguments[^4]

``` emacs-lisp
(if (not (looking-at q-char))
    (if search-forward-char
        (error "Couldn't find any expansion starting with %S" char)
      (goto-char starting-point)
      (setq mark-active nil)
      (change-inner* yank? char))
  ;; … else …
  )
```

During that additional run of the function,
it searches for the correct delimiter via

``` emacs-lisp
(search-forward char (point-at-eol))
```

and<!--
-->—due to the way that `search-forward` works by default—<!--
-->we end up with the point directly after the opening delimiter

``` emacs-lisp
'(| "one" "two" "three" "four" )
```

This now expands correctly.[^3]

## Puni to the rescue

I certainly know what I think of this solution.
Instead of trying to fix this web of expansions and contractions,
how about we rewrite the function instead?

I've been happily using [puni][ghub:puni] for a while,
and it seems pretty apt for the job.
Briefly, puni is a structured editing package,
like [paredit] or [smartparens][ghub:smartparens],
but it works for a broader range of languages than the former,
while comprising of a much smaller code-base<!--
-->—and even fewer language-specific bits—<!--
-->than the latter.[^7]
While I still prefer paredit for lisps,
puni has become my de facto standard for language-agnostic parenthesis handling.

Luckily for us,
puni already comes equipped with a `puni-expand-region` function,
so one can swiftly rewrite the core of `change-inner*` using that instead of `er--expand-region-1`:

``` emacs-lisp
;; Try to find a region.
(puni-expand-region)
(when (> (point) (mark)) ; By default, puni jumps to the end of the sexp
  (exchange-point-and-mark))
(while (and (not (= (point) (point-min)))
            (not (looking-at q-char)))
  (puni-expand-region))
```

Notice that the double expansion vanished!
Instead, when a region was found,
we can make use of `puni-bounds-of-list-around-point` to get the internals explicitly,
and then calculate how big the delimiters were:

``` emacs-lisp
(let* ((rb (region-beginning))
       (re (region-end))
       (insides (progn (goto-char (1+ rb))
                       (puni-bounds-of-list-around-point)))
       (olen (- (car insides) rb))  ; Length of opening delimiter
       (clen (- re (cdr insides)))) ; Length of closing delimiter
  (kill-region (+ rb olen) (- re clen)))
```

Trying this out with our trusty example of

``` javascript
var foo = bar(|
    "baz");
```

we… are greeted with a type error.

    Debugger entered--Lisp error: (wrong-type-argument number-or-marker-p nil)
      puni--smaller-interval((103 . 108) (nil . 108))

Yikes.

## Puni to the rescue?

The `puni--smaller-interval` function does some comparisons with `<=`,
and having `nil` in there will obviously result in a bad time for everyone.
As it turns out, puni *also* has some problems handling whitespace,
in that it *doesn't* skip it.
At some point in `puni-expand-region`,
we call `puni-bounds-of-sexp-at-point`,
which tries to find out whether we are at the start or end of an S-expression
by going forwards and backwards a few times:

``` emacs-lisp
(save-excursion
  (setq end-forward (puni-strict-forward-sexp)
        beg-forward (puni-strict-backward-sexp)))
(save-excursion
  (setq beg-backward (puni-strict-backward-sexp)
        end-backward (puni-strict-forward-sexp)))
```

Now, when we are in a situation like `(|   "furble")`,
an invocation of `puni-strict-forward-sexp` will leave us at `(   "furble"|)`,
but executing `puni-strict-backward-sexp` after that will result in `(   |"furble")`—not where we started.
As such, puni will (incorrectly) conclude that we were not at the start of the expression.

One could try to cram some whitespace handling into this,
but who says we don't run into other issues then?[^13]
In fact, `puni-expand-region` is written in such a way
that it tries out different expansion strategies until one succeeds—why not just quiet the error?

``` emacs-lisp
(advice-add 'puni-bounds-of-sexp-at-point :around
  (lambda (fun)
    (ignore-errors (fun))))
```

This… turns out to work!

``` javascript
// before
var foo = bar(|
    "baz");

// after
var foo = bar(|);
```

Phew.

# The code

For anyone interested, here is the full code.
It also includes a `mode` setting, which can be set to `outer`,
in order to kill around the parentheses; e.g.,

``` rust
// before
let issue_list_url = Url::parse(|"https://my-url.com").unwrap();

// after
let issue_list_url = Url::parse|.unwrap();
```

For obvious reasons,
I will not submit this upstream to change-inner,
but it will instead live in my personal configuration.[^6]

``` emacs-lisp
(cl-defun slot/change-sexp (&key search-for mode)
  "Delete (the innards of) a sexp.
Takes a char, like ( or \", and kills the first ancestor semantic
unit starting with that char. The unit must be recognisable to
`puni'.

SEARCH-FOR is the opening delimiter to search for: if this is
nil, prompt for one. MODE is whether to kill the whole
region (`outer'), or just the innards of it (any other value,
including nil)."
  (cl-labels
      ((expand (char &optional forward)
         "Expand until we encompass the whole expression."
         (let* ((char (or char
                          (char-to-string
                           (read-char (format "Kill %s:"
                                              (symbol-name
                                               (or mode 'inner)))))))
                (q-char (regexp-quote char))
                (starting-point (point)))
           ;; Try to find a region.
           (puni-expand-region)
           (when (> (point) (mark))
             (exchange-point-and-mark))
           (while (and (not (= (point) (point-min)))
                       (not (looking-at q-char)))
             (puni-expand-region))
           ;; If we haven't found one yet, initiate a forward search and
           ;; try again—once.
           (when (not (looking-at q-char))
             (goto-char starting-point)
             (deactivate-mark)
             (if forward
                 (error "Couldn't find any expansion starting with %S" char)
               (search-forward char (pos-eol 2))
               (expand char 'forward))))))
    (expand search-for)
    ;; Now that we have a region, decide what to do with it.
    (let ((rb (region-beginning))
          (re (region-end)))
      (if (eq mode 'outer)
          (kill-region rb re)           ; Kill everything
        ;; If we want to delete inside the expression, fall back to `puni'.
        ;; This circumvents having to call `er--expand-region-1' and then
        ;; `er/contract-region' in some vaguely sensical order, and hoping
        ;; to recover the inner expansion from that.
        ;; Addresses ghub:magnars/change-inner.el#5
        (let* ((insides (progn (goto-char (1+ rb))
                               (puni-bounds-of-list-around-point)))
               (olen (- (car insides) rb)) ; Length of opening delimiter
               (clen (- re (cdr insides)))) ; Length of closing delimiter
          (kill-region (+ rb olen) (- re clen)))))))
```

One can bind killing the innards to `M-i`,
and killing everything to `M-o`,
as change-inner suggests.
Alternatively, and this is what I do,[^1]
a second small helper function is swiftly written,
such that only one keybinding is needed:

``` emacs-lisp
(defun slot/change-around (&optional arg)
  (interactive "P")
  (if arg
      (slot/change-sexp :mode 'outer)
    (slot/change-sexp)))

(bind-key "M-i" #'slot/change-around)
```

[ghub:change-inner#5]: https://github.com/magnars/change-inner.el/issues/5
[ghub:change-inner#9]: https://github.com/magnars/change-inner.el/issues/9
[ghub:change-inner]: https://github.com/magnars/change-inner.el
[ghub:evil-mode]: https://github.com/emacs-evil/evil
[ghub:expand-region]: https://github.com/magnars/expand-region.el
[ghub:puni]: https://github.com/AmaiKinono/puni
[ghub:smartparens]: https://github.com/Fuco1/smartparens
[paredit]: https://paredit.org/

[^1]: `M-o` will never be something other than `other-window`.

[^2]: {-} As is common,
      I will use `|`
      to indicate the position of the point.

[^3]: Actually,
      the searching also has a different, actual, use.
      When in a situation like

      ``` emacs-lisp
      '(1| 2 "this is a string")
      ```

      One might want to change the string—indeed,
      `M-x change-inner "` correctly jumps to the string:

      ``` emacs-lisp
      '(1 2 "|")
      ```

      This is one of the great features of Vim's `ci"`,
      and certainly something to preserve.

[^4]: {-} 󠀠

       󠀠

      `search-forward-char` is the second argument of `change-inner*`;
      if the function was called with that,
      we have already recursed once, so stop.
      `char` is the character that the user actually input.
      `starting-point` is the position of the point before anything happened.

[^5]: {-} 󠀠

       󠀠

     `q-char` is the char that the user input,
      but quoted as a regular expression via `regexp-quote`.

[^6]: {-} 󠀠

       󠀠

       󠀠

       󠀠

       󠀠

       󠀠

       󠀠

       󠀠

       󠀠

      Using a recursive local function also incidentally fixes [#9][ghub:change-inner#9].
      Nice.

[^7]: Puni achieves this by relying on Emacs's built-in functions.

[^9]: I wish I hadn't.

[^10]: {-}  󠀠

       As you've probably already guessed,
       the `^`'s are supposed to signal the marked region.

[^11]: As in

       ``` emacs-lisp
       '|( "one" "two" "three" "four" )
       ; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       ```

[^13]: The real reason,
       of course,
       is that I just wanted my code to work *right now*,
       instead of having to wait for upstream to fix something.
       At some point this should definitely be fixed in puni, though.
