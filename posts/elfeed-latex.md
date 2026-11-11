---
title: LaTeX Previews for Elfeed
date: 2026-11-09
tags: emacs
og-description: Adjusting Org's new LaTeX preview system for elfeed.
---

Org's new LaTeX preview system
[is](https://karthinks.com/software/understanding-org-latex-preview/)
[out](https://blog.tecosaur.net/tmio/2026-11-01-preview.html)—time to celebrate!
Not only does this mean that one can now enjoy reasonable previews without having to use a forked version of Org,
but given that we are using the infinitely extensible text editor,
this preview functionality can be used [virtually everywhere](https://www.youtube.com/watch?v=u44X_th6_oY).
Let's hack it into elfeed.

<!--more-->

While its creator seems to [disagree](https://nullprogram.com/blog/2026/04/26/),
I still believe that [elfeed](https://github.com/emacs-elfeed/elfeed) is by far the best feed reader in existence.[^5]
As RSS feeds are so static, it also feels like the ideal candidate to get a feeling for adapting `org-latex-preview` to other major modes.[^2]

Previewing LaTeX is quite important when one is subscribed to arXiv's feeds,[^1]
as titles and descriptions obviously contain maths sometimes:

<p><video width="100%" controls>
  <source src="../images/elfeed-latex/elfeed.webm" media="(prefers-color-scheme: light)" type="video/webm">
  <source src="../images/elfeed-latex/elfeed-dark.webm" media="(prefers-color-scheme: dark)" type="video/webm">
 </video></p>

The only function that we'll need to get these previews working is `org-latex-preview-place`.
It has two mandatory arguments, `processing-type` and `entries`.
The latter is just a list of buffer positions `(beg end)` in which to find the LaTeX fragments,
while the former sets the type of transformation you want to use.[^3]
This means that the main thing we have to do is to collect all of the fragments:

``` emacs-lisp
(defun elfeed-latex-preview ()
  "LaTeX preview the current elfeed buffer."
  (interactive)
  (save-excursion
    (with-syntax-table (copy-syntax-table)
      (modify-syntax-entry ?- ".") ; $x$-y is valid
      (when-let* ((entries
                   (cl-loop
                    for (_ re pos _) in org-latex-regexps
                    append (cl-loop
                            initially (goto-char (point-min))
                            while (re-search-forward re nil t)
                            collect (list (match-beginning pos)
                                          (match-end pos))))))
        (org-latex-preview-place 'dvisvgm entries)))))
```

The `cl-loop` code is so close to natural language that I feel like it should be obvious even if you've never seen such a loop expression.
The only thing that's slightly tricky is that Org doesn't really recognise constructs of the form `$x$-y` as valid
LaTeX—as far as I know because it conflicts with one of its million syntax rules.
To hack around this, as I still want to use `org-latex-regexps`,[^4]
we can treat the dash as punctuation while looking for fragments, since a punctuation character following the closing dollar is permitted.

To hook this function into elfeed, we can simply check if the arXiv view is selected in either `elfeed-search-mode` or `elfeed-show-mode`,
and then just call `elfeed-latex-preview` on the buffer:

``` emacs-lisp
(defun elfeed-latex (&rest _)
  (when (or (string-match-p "arXiv" elfeed-search-filter)
            (and elfeed-show-entry
                 (memq 'arXiv (elfeed-entry-tags elfeed-show-entry))))
    (elfeed-latex-preview)))

(add-hook 'elfeed-search-update-hook #'elfeed-latex)
(add-hook 'elfeed-show-update-hook   #'elfeed-latex)
(advice-add 'elfeed-search-update-entry :after #'elfeed-latex)
```

As the names suggest, the two hooks are for when things change in `elfeed-search-mode` and `elfeed-show-mode`, respectively.
The hooks for entering those major modes seem to run too early for LaTeX previewing to have any effect.
The advice is needed since opening a feed entry with `elfeed-search-show-entry` does update and redraw its title (e.g., marking it read),
but sadly elfeed bypasses the usual `search-update` machinery, and thus this doesn't activate any of the required hooks.

Pretty feeds, at last.

[^1]: If you can [stomach the arXiv in 2026](https://arxiv.org/stats/monthly_submissions), that is.
      I know enough people who've just given up at this point,
      and are just scanning whose name they recognise.
      While this usually *is* a good way to filter out slop, it's honestly quite worrying—<!--
      -->but that's not what this post is supposed to be about, so let's stop here.

[^2]: Not that it makes much of a difference; the API as I see it is essentially plug-and-play.

[^3]: `dvisvgm`, `dvipng`, or `imagemagick`, at the time of writing.

[^4]: Yes, the subset of TeX allowed in arXiv titles and descriptions is so minimal that one could probably get away with manually curated regexps.
      However, this would result in more code, so this approach seems simpler.
      It also has the nice benefit that some people don't care what the arXiv allows or not, and will use `\(…\)`-style brackets, even though those are not rendered on the website.

[^5]: Certainly better than a vibecoded C++ GUI thing...
