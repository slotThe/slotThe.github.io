---
title: Integrating Zsh's History Into Eshell
date: 2023-11-27
tags: emacs
no-toc: true
---

I use eshell as my main shell.
Still,
a terminal emulator with zsh is kept around for longer running processes and [scratchpads][XMonad.Util.NamedScratchpad].
One thing that's essential for this setup to make sense is that eshell and zsh share the same history file.
Sadly,
this doesn't work out of the box:
zsh stores its history in a metafied format<!--
-->—nothing that's not fixable on the Emacs side, of course!

[XMonad.Util.NamedScratchpad]: https://hackage.haskell.org/package/xmonad-contrib/docs/XMonad-Util-NamedScratchpad.html

<!--more-->

[This email][zsh:mailing-list:metafied] explains the problem quite well.
In short:

> […] when 0x80-0x9F characters are used, then always 0x83 Meta
> character is inserted and following character is bit shifted, […]

This is not a bug, but expected behaviour;
zsh's history is saved in exactly such a metafied format.
The upshot is that,
when setting `eshell-history-file-name` to e.g. `"~/.config/zsh/zsh_history"`,
Emacs either won't know how to properly encode the file upon exiting eshell,
or one will get suggestions<!--
-->—using a package like [esh-autosuggest]—<!--
-->containing garbage like `\304§` instead of `ć`.

The linked discussion helpfully contains a small C program to unmetafy the history:

``` c
#define Meta ((char) 0x83)

/* from zsh utils.c */
char *unmetafy(char *s, int *len)
{
  char *p, *t;

  for (p = s; *p && *p != Meta; p++);
  for (t = p; (*t = *p++);)
    if (*t++ == Meta)
      t[-1] = *p++ ^ 32;
  if (len)
    *len = t - s;
  return s;
}
```

This looks pretty terse,
at least to my non-C-reading-eyes,
but essentially every time `0x83` is encountered,
we delete it and XOR the following character with the number 32.
An implementation in elisp might look like the following:

``` emacs-lisp
(defun slot/unmetafy ()
  (cl-flet ((unmetafy (input)
              (let ((i 0) output)
                (while-let ((char (nth i input))
                            (inc-and-char
                             (if (= char #x83)
                                 ;; Skip meta character and unmetafy.
                                 `(2 . ,(logxor (nth (1+ i) input) 32))
                               ;; Advance as usual.
                               `(1 . ,char))))
                  (cl-incf i (car inc-and-char))
                  (setq output (cons (cdr inc-and-char) output)))
                (decode-coding-string
                 (apply #'unibyte-string (nreverse output))
                 'utf-8-unix
                 t))))
    (let ((hist-file "~/.config/zsh/zsh_history"))
      (with-temp-buffer
        (insert (mapconcat (-compose #'unmetafy #'string-to-list)
                           (s-lines (f-read-bytes hist-file))
                           "\n"))
        (write-file hist-file)))))

```

This can be conveniently integrated into an `eshell/exit`-like function,
such as

``` emacs-lisp
(defun slot/eshell-exit (&optional arg)
  "Exit eshell and kill the current frame."
  (interactive "P")
  (slot/unmetafy)
  (eshell-write-history)
  (save-buffers-kill-terminal))

```

Finally, one just need to take care to bind that function to a key,
and to unmetafy the history when eshell starts.[^1]

``` emacs-lisp
(use-package eshell
  :hook (eshell-hist-load . slot/unmetafy)
  :bind (:map eshell-mode-map
              ("C-x C-c" . slot/eshell-exit)))

```

[zsh:mailing-list:metafied]: https://www.zsh.org/mla/users/2011/msg00154.html
[esh-autosuggest]: https://github.com/dieggsy/esh-autosuggest

[^1]: Unmetafying when quitting resolves the "Emacs can't decide on an encoding" issue,
      and doing so at the start solves `esh-autosuggest` et al producing garbage suggestions.
