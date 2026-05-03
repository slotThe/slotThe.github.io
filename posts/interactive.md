---
title: Writing Literate Blog Posts
date: 2026-04-23
last-modified: 2026-04-23
tags: emacs
og-description: Hacking on Org Babel and export, for hakyll/pandoc integration.
---

Let's try to contort enough Emacs packages to allow for a smooth Org to
GFM⁺⁺ export, so I can write literate programs in the comfort of Org
mode.

<!--more-->

This website is powered by hakyll, which eventually hands off to pandoc
to do all of the <abbr title="(Microsoft) GitHub-flavoured
markdown">GFM</abbr> to HTML mangling. I have a [moderately
involved](https://github.com/slotThe/slotThe.github.io/blob/main/src/site.hs)
hakyll configuration, which in particular means I'll not be moving to a
purely Emacs-based setup anytime soon. However, the mere existence of
Org mode is a straight upgrade for certain things, which I'd like to
leverage somehow.

For this post, I'll concentrate on "literate blog posts", in the sense
of [literate
programming](https://en.wikipedia.org/wiki/Literate_programming).[^9]
Basically, this is about having executable code blocks interspersed with
prose, explaining what's going on, with the niceties that later code
blocks can refer to earlier ones, and so on. An example is [Sudoku
Solving in BQN](/posts/bqn-sudoku.html), which was written with this
kind of setup (as one can see by the [associated Org
file](https://github.com/slotThe/slotThe.github.io/blob/main/posts/bqn-sudoku.org)
in the repository). As getting the Org to HTML export exactly right
sounds like a huge headache, it seems easier to go from Org to GFM
inside of Emacs, and then have pandoc take over for the GFM to HTML
step. Hence, I will focus mostly on wrangling with Babel to generate
REPL-like blocks, as well as with Org's export machinery to sanely
translate the blocks to markdown.

---

The techniques used here work for essentially all languages that come
with some kind of way to evaluate code in a REPL. If you're curious,
I've personally tried this with [BQN](https://mlochbaum.github.io/BQN/)
and [growler/k](https://codeberg.org/growler/k). If you want, you can
peruse the resulting [ob-bqn.el](https://codeberg.org/slotThe/ob-bqn),
[ob-k.el](https://codeberg.org/slotThe/ob-k), and
[ox-gfm](https://codeberg.org/slotThe/ox-gfm) packages.

# Babel

[^8]You can think of [Org
Babel](https://orgmode.org/worg/org-contrib/babel/intro.html) as a bit
like [Jupyter Notebooks](https://jupyter.org/), but for any language,
and on steroids. It's Org mode's machinery for working with source code
blocks, and there's a lot of cool functionality packed into it: blocks
can share state, pipe their output into other blocks, be exported with
or without their results, and so on. For this application, we're mostly
going to focus on basic evaluation, as well as exporting. For example,

``` org
#+BEGIN_SRC python :results output
  print("beep boop")
#+END_SRC
```

will, upon executing the fantastically named `org-ctrl-c-ctrl-c` with
`C-c C-c`, yield a results block underneath it.[^6]

``` org
#+RESULTS:
: beep boop
```

In the very unlikely case that you're using a language that's not
covered by an existing Babel package, either
[officially](https://orgmode.org/worg/org-contrib/babel/languages/index.html)
or on some random repository,[^2] it's actually pretty easy to write a
package yourself. All that's needed is an `org-babel-execute:«lang»`
function, which tells Org how to evaluate code for your language, and
Org takes care of the rest. There's even an official
[template.el](https://git.sr.ht/~bzg/worg/tree/master/item/org-contrib/babel/ob-template.el)
available, which one can use to get up and running a bit quicker, and
the
[docs](https://orgmode.org/worg/org-contrib/babel/languages/index.html)
are also—as always—informative to read.

## REPL blocks

For the specific posts I'm writing, I'm looking for more of an
interactive experience. Basically, I want to simulate a REPL, where,
say, the input is indented by a certain number of spaces, and the output
is flush to the left:

``` bqn
   i←"10+((5+42)+8)×(3-(24+5))"
"10+((5+42)+8)×(3-(24+5))"
   (¯1+d×+`»⊸<d←i∊'0'+↕10)⊔i
⟨ "10" "5" "42" "8" "3" "24" "5" ⟩
```

This more or less just uses building blocks that are already present in
Org mode and rearranges them: the Org block looks like

``` org
#+BEGIN_SRC bqn :results repl :exports results :wrap SRC bqn
  i←"10+((5+42)+8)×(3-(24+5))"
  (10⊸×⊸+˜´·⌽-⟜'0')¨(¯1+d×+`»⊸<d←i∊'0'+↕10)⊔i
#+END_SRC
```

The `:exports results` and `:wrap SRC bqn` directives should hopefully
be self-explanatory. The `repl` results type is a very simple twist on
the execution of a block, wherein I just send one line per block to the
REPL, await the result, and print that right below the line. The
ordinary `bqn-mode` already integrates with `comint`, so one can just
reuse the respective function for this application.

``` emacs-lisp
(defun org-babel-bqn--execute-repl (body)
  "Execute BODY line-by-line, returning input/output pairs."
  (let ((lines (split-string body "\n" t "[ \t]+"))) ; trim whitespace, drop empty
    (mapconcat
     (lambda (line)
       (format "   %s\n%s" line (bqn-comint-evaluate-command line)))
     lines
     "\n")))
```

Threading that through to the execution function works by just matching
on the correct result parameter.

``` emacs-lisp
(defun org-babel-execute:bqn (body params)
  "Execute a block of BQN code with org-babel.
When PARAMS includes `:results repl', evaluate each line separately
and return all results interleaved."
  (let ((result-params (cdr (assq :results params))))
    (if (and result-params (string-match-p "\\brepl\\b" result-params))
        (org-babel-bqn--execute-repl body)
      (bqn-comint-evaluate-command body))))
```

Executing the block results in

``` org
#+RESULTS:
#+begin_SRC bqn
   i←"10+((5+42)+8)×(3-(24+5))"
"10+((5+42)+8)×(3-(24+5))"
   (10⊸×⊸+˜´·⌽-⟜'0')¨(¯1+d×+`»⊸<d←i∊'0'+↕10)⊔i
⟨ 10 5 42 8 3 24 5 ⟩
#+end_SRC
```

All we have to do then is to export the results, wrapped in the right
src block, so that the finished markdown will exactly be

    ``` bqn
       i←"10+((5+42)+8)×(3-(24+5))"
    "10+((5+42)+8)×(3-(24+5))"
       (10⊸×⊸+˜´·⌽-⟜'0')¨(¯1+d×+`»⊸<d←i∊'0'+↕10)⊔i
    ⟨ 10 5 42 8 3 24 5 ⟩
    ```

Given a sufficiently good Markdown export package, Org's machinery now
just works™ on my machine®.

---

One convenience function that I ended up using a lot is to execute all
(named) src blocks in a file, except those that are inside of a results
block.

``` emacs-lisp
(defun org-babel-bqn-execute-named-blocks ()
  "Execute named src blocks not part of a #+RESULTS block.
This may be useful when using `:results repl', and wrapping the
resulting block in a BQN src block again."
  (interactive)
  (org-babel-map-src-blocks nil
    (when (and (string-equal "bqn" (car (org-babel-get-src-block-info 'no-eval)))
               (not (progn (goto-char beg-block)
                           (forward-line -1)
                           (looking-at-p "#\\+RESULTS:"))))
      (goto-char beg-block)
      (org-babel-execute-src-block))))
```

This is great for only executing the blocks I actually want to be
"literate", while leaving the others alone.

# Exporting

Another nook of Org that's worth spending a weekend on is
[exporting](https://orgmode.org/manual/Exporting.html). The gist is that
writing exporters in Emacs can have many advantages over using more
generic programs like pandoc, as Org itself is reasonably complicated,
and introspection is actually good sometimes.

For exporting from Org to GFM there's already
[ox-gfm.el](https://github.com/larstvei/ox-gfm). This already does the
bulk of the work; however, it's not quite specialised enough for my
purposes, and does seem to be abandoned.[^1] As such, I decided to fork
it, and hack in some changes myself.

## Headers

One thing that one needs to teach `ox-gfm` is the YAML-esque headers
that hakyll uses; every post begins with a quick list of the most
salient data:

    ---
    title: Writing Literate Blog Posts
    date: 2026-03-02
    tags: emacs
    ---

Defining an export backend based on another one is done using the
`org-export-define-derived-backend` function.[^3] It takes a name, the
parent it builds on, and a handful of keyword arguments that describe
how the two differ. Everything the parent already knows how to translate
is inherited for free. For example, `ox-gfm` inherits from the builtin
`ox-md`, which in turn inherits from `ox-html`, so in particular this
will be the fallback if nothing else matches.

There are a few possible keyword arguments to the function, so I'd
encourage you to peruse `C-h f org-export-define-derived-backend RET`.
The ones we're interested in are `:translate-alist`, which can attach
exporting functions to smaller elements of the format (tables, code,
footnotes, you name it), and `:options-alist`, which defines the [export
options](https://orgmode.org/manual/Export-Settings.html) accepted by
Org.

To not keep you in unnecessary suspense, here's what we need to actually
add to the existing backend derivation:

``` emacs-lisp
(org-export-define-derived-backend 'gfm 'md
  ; …
  :options-alist
  '((:tags "TAGS" nil nil split)
    (:last-modified "LAST-MODIFIED" nil nil)
    (:og-description "OG-DESCRIPTION" nil nil))
  :translate-alist '((template . org-gfm-template)
                     ; …
                     ))
```

Some of hakyll's metadata fields weren't known to Org,[^5] and for
translating this into a YAML-style header at the top of the document, we
need to add a function to translate the *template* of an Org document.
This gets the final converted document as an input, so it's the
canonical place for a pre- or postamble. It's fine to just slap in a new
definition here; `ox-gfm` doesn't override it by default, and in
`ox-md`—which does do that—it's defined as the identity function.

Each list element given to `:options-alist` is comprised of `(ALIST-KEY
KEYWORD OPTION DEFAULT BEHAVIOR)`; see the docs of
`org-export-options-alist` for more information. Briefly, `ALIST-KEY` is
the key under which the value ends up in the export `info` plist;
`KEYWORD` is the keyword the user writes into the document; and
`BEHAVIOR` tells Org how to handle a single option having multiple
values, if any. The builtin `split` already does exactly what I want for
`tags`,[^4] and for the others I'm fine with the default behaviour,
which is overwriting.

The template function just slurps out the arguments we care about, and
puts them at the very top of the document.

``` emacs-lisp
(defun org-gfm--build-yaml (info)
  "Build YAML front matter string from INFO plist.
Returns nil if no fields have values."
  (when-let* ((lines
               (seq-keep
                (lambda (f)
                  (when-let* ((field (plist-get info f))
                              (val (pcase f
                                     (:title          #'car)
                                     (:date           #'car)
                                     (:tags           (lambda (x) (mapconcat #'identity x " ")))
                                     (:last-modified  #'identity)
                                     (:og-description #'identity))))
                    (format "%s: %s"
                            (string-trim (pp-to-string f) ":" "\n")
                            (funcall val field))))
                '(:title :date :last-modified :tags :og-description))))
    (concat "---\n" (mapconcat #'identity lines "\n") "\n---\n\n")))

(defun org-gfm-template (contents info)
  "Return complete document string after GFM conversion.
CONTENTS is the transcoded contents string.  INFO is a plist holding
export options."
  (concat (org-gfm--build-yaml info) contents))
```

## Footnotes

The export machinery already knows about footnotes, but since this site
uses [sidenotes](/posts/block-sidenotes.html), I had to adjust the
exporting a tad.

First of all, by default the label of an Org footnote, which looks like
`[fn:1]`, will not get translated to a GFM-style `[^1]`, but directly
into HTML:

```
<sup><a id="fnr.1" class="footref" href="#fn.1" role="doc-backlink">1</a></sup>
```

To fix this, we just need to add `(footnote-reference .
org-gfm-footnote-reference)` to the translate alist of our backend,
where the mentioned function just gets the footnote number and
translates it:

``` emacs-lisp
(defun org-gfm-footnote-reference (footnote-reference _contents info)
  "Transcode a FOOTNOTE-REFERENCE element into GFM format.
_CONTENTS is nil.  INFO is a plist holding contextual information."
  (format "[^%d]" (org-export-get-footnote-number footnote-reference info)))
```

While I do want the content of the footnotes to be there at the end of
the file, I don't want a big `Footnotes` section header, as the
exporting will grab and move them anyways. Thus, I adjusted the already
existing `org-gfm-footnote-section` to

``` emacs-lisp
(defun org-gfm-footnote-section (info)
  "Format the footnote section.
INFO is a plist used as a communication channel."
  (and-let* ((fn-alist (org-export-collect-footnote-definitions info)))
    (format "%s\n"
            (mapconcat (pcase-lambda (`(,n ,_type ,def))
                         (format "[^%d]: %s" n (org-trim (org-export-data def info))))
                       fn-alist
                       "\n\n"))))
```

This is called in `org-gfm-inner-template`—the function that stitches
the document body together—but that call-site does not have to be
adjusted at all.

# Conclusion

With just a few patches to already existing libraries, I can now write
"literate" articles in Org, export them to Markdown with a single key
combination, and have the usual hakyll+pandoc machinery take over. A big
win in ergonomics, certainly: it saves me having to copy everything from
an actual REPL into the file, hoping I won't forget to update an earlier
block if a variable name changes. Indeed, this might actually inspire me
to write more of that flavour of post, which is what the whole thing is
all about, I guess.

[^1]: Or just feature complete, I can't tell.

[^2]: I've not seen this happen for any language I know, btw,
      though if you wander too far outside of the mainstream you might have to go hunting inside of other people's configurations instead of just searching on {NonGnu,M}ELPA.

[^3]: There are so many export backends that this will essentially always be the case when defining a new one. Your file format is not as unique as you think it is.

[^4]: This allows several tags to just be specified by comma separation: `#+tags: array-lang, c, k`.

[^5]: Not the case for `:title` or `:date`; even though I'm using them later on, Org already knows about their existence.

[^6]: The `:results output` directive is an additional instruction to Org, which returns whatever is displayed on stdout. By default, Org wraps your code in a function, calls that function, and displays the return value. If one uses IO, this obviously doesn't really display what one wants.

[^8]: {-} If you already know what Org Babel is, feel free to skip to [REPL blocks](#repl-blocks).

[^9]: [Some languages](https://wiki.haskell.org/index.php?title=Literate_programming) even have extra support for this!
