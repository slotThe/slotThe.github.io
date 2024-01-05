---
title: Parentheses-Aware Yanking
date: 2024-01-03
tags: emacs
---

<p></p>
Copying, killing, and yanking[^5] text in Emacs is quite straightforward,
and very little is done to manipulate the contents of the  string during these operations.
Rightfully so, of course,
as this might yield pretty surprising behaviour to many people.
To me, however, inserting unbalanced expressions is even more surprising
than not preprocessing anything at all, so let's change that.

<!--more-->

# The problem

Say you have the following s-expression:

``` emacs-lisp
(insert-for-yank
 (current-kill
  (cond
   ((listp arg) 0)
   ((eq arg '-) -2)
   (t (1- arg)))))
```

Now, for whatever reason,
you might want to copy the `(t (1- arg))` on the last line.
One way to do that would be to navigate to it,
set the mark with `M-SPC`,
jump to its end with `M-C-f`,
and save everything to the kill ring with `M-w`.[^6]
This works, but it feels a bit slow sometimes,
even when navigating to the expression is fast thanks to
[paredit](https://paredit.org/).

There is the wonderful
[whole-line-or-region](https://github.com/purcell/whole-line-or-region)
package,
which makes many commands act on the current line if no region is selected.
With that, copying the last line of the above expression just involves navigating to it and pressing `M-w`.
Alas, when yanking the line into the buffer again, we are greeted with

``` emacs-lisp
(t (1- arg)))))
```

Lots of unbalanced parentheses!
This is especially troublesome when one uses
[aggressive-indent-mode](https://github.com/Malabarba/aggressive-indent-mode)<!--
-->—as I do—since then you run the chance of your buffer shifting around you.

All of this would be fixed with a version of `yank` that correctly trims off these extraneous parentheses
before yanking the killed text into the buffer.
Let's make one.[^7]

# An approximate solution

We first create a small function that gives us some info about how many
open delimiters[^1] there are in the current buffer.
The implementation is straightforward,
we just need to make sure to take care of a few special cases<!--
-->—and I almost certainly missed one—<!--
-->like delimiters appearing inside of strings or comments.[^8]

``` emacs-lisp
(defun slot/get-delimiters ()
  "Return delimiter count in current buffer.
Returns a list, each element being of the form (OPEN CLOSE AMNT),
where OPEN and CLOSE are the respective opening and closing
delimiters, and AMNT is an integer; a positive (negative) number
signalling that there are that many extraneous opening (closing)
delimiters.  Thus, a value of 0 signifies a balanced buffer.

Do not count a delimiter towards the global total if it is
escaped (prefixed by a backslash), part of a string, or part of a
comment."
  (goto-char (point-min))
  (let-alist '((paren . 0) (square . 0) (curly . 0))
    (while-let ((char (char-after)))
      (unless (or (-intersection (text-properties-at (point))
                                 '(font-lock-string-face
                                   font-lock-comment-face))
                  (eq ?\\ (char-before)))
        (pcase char
          (?\( (cl-incf .paren)) (?\[ (cl-incf .square)) (?\{ (cl-incf .curly))
          (?\) (cl-decf .paren)) (?\] (cl-decf .square)) (?\} (cl-decf .curly))))
      (forward-char))
    `(("(" ")" ,.paren)
      ("[" "]" ,.square)
      ("{" "}" ,.curly))))
```

Having `slot/get-delimiters`, which does most of the actual work,
all that's left to get this running is to actually trim the string before yanking it.
No tricks needed here.

``` emacs-lisp
(defun slot/trim-delimiter (open close n)
  "Trim delimiter in current buffer.
OPEN and CLOSE are the respective opening and closing delimiters.
The number N indicates how many—and which—delimiters to trim.  If
it is positive, trim CLOSE; otherwise, trim OPEN."
  (-let (((pt del) (if (< n 0)          ; More closing than opening?
                       `(point-max (when (search-backward ,close (point-min) t)
                                     (delete-forward-char 1)))
                     `(point-min (when (search-forward ,open (point-max) t)
                                   (delete-backward-char 1))))))
    (goto-char (funcall pt))
    (dotimes (_ (abs n))
      (eval del))))

(defun slot/trim-delimiters (str)
  "Trim delimiters in current buffer.
See `slot/get-delimiters' for a list of all relevant delimiters,
and `slot/trim-delimiter' for how delimiters are actually
trimmed."
  (with-temp-buffer
    (insert str)
    (--each (slot/get-delimiters)
      (apply #'slot/trim-delimiter it))
    (buffer-string)))

;; Implementation copied verbatim from `yank', except for the insertion
;; of `slot/trim-delimiters'.
(defun slot/yank (&optional arg)
  "Delimiter-aware yanking.
Like `yank' (which see), but trim non-matching delimiters from
the string before actually yanking it into the current buffer.
The kill-ring itself remains untouched."
  (interactive "*P")
  (setq yank-window-start (window-start))
  ;; If we don't get all the way through, make `last-command' indicate
  ;; that for the following command.
  (setq this-command t)
  (push-mark)
  (insert-for-yank (slot/trim-delimiters        ; <- HERE
                    (current-kill
                     (cond
                      ((listp arg) 0)
                      ((eq arg '-) -2)
                      (t (1- arg))))))
  (when (consp arg)
    ;; This is like `exchange-point-and-mark', but doesn't activate
    ;; the mark.  It is cleaner to avoid activation, even though the
    ;; command loop would deactivate the mark because we inserted text.
    (goto-char (prog1 (mark t)
                 (set-marker (mark-marker) (point) (current-buffer)))))
  ;; If we do get all the way through, make `this-command' indicate that.
  (when (eq this-command t)
    (setq this-command 'yank))
  nil)
```

Instead of defining `slot/yank`,
one might also just override `yank` with the new definition.
That, however, seems a bit uncouth,
and it's not terribly difficult to assign the same key to the new function:

``` emacs-lisp
(bind-key "C-y" #'slot/yank)
```

Killing `(t (1- arg)))))` and yanking it again yields `(t (1- arg))`,
as desired.

[^1]: By "delimiter" I mean parentheses, square brackets, and curly braces.

[^5]: Also known as copying, cutting, and pasting in every other context ever.

[^6]: One might also use `C-k C-y` (`paredit-kill` followed by `yank`) instead of `M-SPC M-C-f M-w`,
      but that only works at the end of an expression.
      Plus, what `paredit-kill` kills might be more than the current line.

[^7]: Doing it this way also has the added benefit of working for every major mode,
      not just ones where I happen to use paredit
      (or [puni](https://github.com/AmaiKinono/puni)).

[^8]: {-} 󠀠

      󠀠

      󠀠

      This code now also lives [here](https://github.com/slotThe/yank-delimiters).
