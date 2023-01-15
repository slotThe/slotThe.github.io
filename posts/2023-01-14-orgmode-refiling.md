---
title: Immediately Refile Notes with X.P.OrgMode
tags: emacs, xmonad
---

In a [previous post][orgmode-post] I talked about using
[XMonad.Prompt.OrgMode] to rapidly capture thoughts and ideas into an
Org file.  The functionality that the module provides has proven to be
extremely useful to me, and really I couldn't be happier with
it. However, a user recently contacted me by email and told me that
they're missing but one feature: the ability to immediately refile
notes.

[XMonad.Prompt.OrgMode]: https://xmonad.github.io/xmonad-docs/xmonad-contrib/XMonad-Prompt-OrgMode.html
[orgmode-post]: /posts/orgmode-prompt/2022-08-27-xmonad-and-org-mode.html

<!--more-->

# Motivation

If you don't know, [refiling] is the act of moving an entry[^1] *below*
another heading; i.e., such that it becomes a subheading there.  This
can be useful for structuring TODOs into separate categories: one might
have projects called "Work", "Life", "XMonad", and so on, where all
related tasks live.  Quite convenient!

So far, X.P.OrgMode just dumped the created note at the end of the
specified file, leaving you to pick up the pieces.  This aligns with my
personal workflow—while I extensively use refiling, I only do so at the
end of the day after reviewing all tasks that have accumulated.
However, it is conceivable that someone might want to refile certain
tasks straight away when it's pretty clear that (i) they'll be kept, and
(ii) they can be unambiguously assigned to a certain heading (e.g., an
already scheduled work meeting with X).

# Showcase

Long story short, this is now built into X.P.OrgMode.  There are two new
functions:

``` haskell
    orgPromptRefile   :: XPConfig ->           String -> FilePath -> X ()
    orgPromptRefileTo :: XPConfig -> String -> String -> FilePath -> X ()
```

The former takes the same arguments as [orgPrompt] (which see), and is
for popping up another prompt that asks for a heading.  The latter
refiles everything under the specified (as the second argument) heading.

The way `orgPromptRefile` works is that, after querying for a TODO, it
*always* inserts the note into the file and then *possibly* refiles it
to another heading.  This way, you don't need to worry about losing
notes when you abort the refiling prompt or enter a non-existent
heading.

<img class="pure-img" src="../images/orgmode-refiling/refiling.gif">

Note: Refiling is (near) instant; the delay you are seeing above is due
to `auto-revert-mode`.

## Some Gory Details

All of the refiling is actually directly done by Emacs itself!  More
precisely, the EDSL that [XMonad.Util.Run] defines—which I've also
[written about][calling-emacs-from-xmonad]—shells out to Emacs.  This
might intuitively *feel* horrible, but that's just another reason to
share it:

``` haskell
refile :: String -> FilePath -> X ()
refile (asString -> parent) (asString -> fp) =
  proc $ inEmacs
     >-> asBatch
     >-> eval (progn
                [ "find-file" <> fp
                , "end-of-buffer"
                , "org-refile nil nil"
                    <> list [ parent, fp, "nil"
                            , saveExcursion
                               ["org-find-exact-headline-in-buffer" <> parent]
                            ]
                , "save-buffer"
                ])
```

This—as you probably guessed already—just executes the following elisp
snippet in Emacs's batch mode:

``` lisp
(progn
  (find-file «fp»)
  (end-of-buffer)
  (org-refile nil nil
              (list «parent» «fp» nil
                    (save-excursion
                      (org-find-exact-headline-in-buffer «parent»))))
  (save-buffer))
```

I know this seems insane, but letting Emacs do this work is actually
much less brittle than the alternative.  The Org maintainers certainly
know best what refiling *means*, and thus also what it entails—if all of
this logic is already written, why not take advantage of it?  Plus, I
now don't have to keep track of subtle changes in newer versions of Org.

# Closing Thoughts

Writing this was actually a lot of fun, and a great opportunity to play
with the EDSL that X.U.Run exposes.  I reckon there are a few places in
my own XMonad configuration in which I could use these kinds of "Emacs
scripts" to great effect!

One other idea I've had is to integrate this into the language that
plain old `orgPrompt` accepts.  It could be prefixed by something like
"`ref:`", followed by a unique substring with which to identity a
heading.  This would have the disadvantage that—without the second
prompt—one would not get any suggestions for headings.  However, if you
want to refile something you probably know where you want to put it;
plus, it would not involve a possibly distracting second prompt.
Actually, this sounds like a good first pull request: contributions
welcome!

[XMonad.Util.Run]: https://hackage.haskell.org/package/xmonad-contrib/docs/XMonad-Util-Run.html
[XMonad.Util.Run]: https://hackage.haskell.org/package/xmonad-contrib/docs/XMonad-Util-Run.html
[calling-emacs-from-xmonad]: /posts/2022-05-25-calling-emacs-from-xmonad.html
[orgPrompt]: https://hackage.haskell.org/package/xmonad-contrib-0.17.1/docs/XMonad-Prompt-OrgMode.html#v:orgPrompt
[refiling]: https://orgmode.org/manual/Refile-and-Copy.html

[^1]: A headline, possibly with an attached body.
