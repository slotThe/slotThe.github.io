---
title: Calling Emacs from XMonad
tags: emacs, xmonad
---

In the post about my [research workflow], I briefly mentioned having to
call Emacs—or other external programs—from within XMonad.  I figured
that this was perhaps something that could be of use to more people than
just me.  After a little bit of deliberation and coming up with a
generic enough API, I decided to turn it into an XMonad module!

Yesterday these changes were merged into the [xmonad-contrib] repository
and they are now available for everyone to try out; provided one has the
git version of XMonad [installed](https://xmonad.org/INSTALL.html).[^5]

I'd like to use this opportunity to both showcase the module—how and why
one would use it—and talk a little bit about its only redeeming
implementation detail.

<!--more-->

# Main Use Cases

Wouldn't it be neat to have some kind of [EDSL] for spawning external
processes?  Something where one can just compose Haskell functions
together, not having to worry about the actual underlying string
manipulations?  Something that's composable, so that one does not have
to write the same `"emacsclient -c -a '' …"` or `"alacritty
--working-directory …"` prefix over and over again?  Well, at least
that's what I thought on some rainy afternoon a few months ago.

## Scratchpads

The first use case that I came up with was [scratchpad]s.  The idea of
these things is very simple: while we normally don't like floating
windows, it's quite convenient to have some of them around that one can
bring to the current workspace, as well as hide, with a single
keybinding.  This is very useful for things like email, a calendar, a
daily agenda, a calculator, etc.

For scratchpads to work reliably, windows need to have some unique
characteristic they can be recognised by, like a special [class or
instance name].  Endowing an application with additional properties
sounds exactly like what our EDSL should be good at, so let's try that!

Using the new features of `XMonad.Util.Run`, we could spawn an Emacs
frame with a special name for our scratchpad hooks to grab onto, and
execute `notmuch`:

``` haskell
mailSession :: X String
mailSession = getInput $
  inEditor >-> setFrameName mailInstName
           >-> eval (function "notmuch")
```

You can read the `>->` operator a little like a pipe, where you start
with what you want and thread that information through to the end: "I
want an editor with a certain frame name that also starts up notmuch".

In full, the above function would produce the string (broken into a few
lines for better readability)

```
"emacsclient -c -a ''
             -F '(quote (name . \"notmuch-scratch\"))'
             --eval '(notmuch)'"
```

which would be quite bothersome to type indeed.

Because the type of `mailSession` is `X String` and not just `String`,
the setup for this is a little bit different than usual when using
scratchpads.  You would use it like this:

``` haskell
myScratchpads :: X [NamedScratchpad]
myScratchpads = do
  -- First, get the finished string.
  mailSession <- getInput $
    inEditor >-> setFrameName mailInst >-> eval (elispFun "notmuch")
  -- Now we can insert it into our scratchpads as normal.
  pure [ NS "Mail" mailSession (appName =? mailInst) quake ]
 where
  mailInst = "notmuch-scratch"
  quake    = customFloating $ RationalRect 0 0 1 (4 / 5)

-- The call to @namedScratchpadManageHook@ in the manageHook also
-- needs to be slightly adjusted.
myManageHook :: ManageHook
myManageHook = mconcat
  [ …
  , namedScratchpadManageHook =<< liftX myScratchpads
  ]
```

Normally you would also add your `myScratchpads` list to all calls of
`namedScratchpadAction`; e.g., when you define the keys to call your
scratchpads.  However, since the former lives in `X` now, this doesn't
work anymore!  Thankfully,
[nowadays](https://github.com/xmonad/xmonad-contrib/commit/3fc830aa09368dca04df24bf7ec4ac817f2de479)
the first argument to `namedScratchpadAction` is actually unused and
only there for backwards compatibility.  This means that it's not
necessary to enter your scratchpads there at all if they are added to
your `manageHook`.  For example, in the following I just provide the empty list:

``` haskell
  ("M-C-t", namedScratchpadAction [] "Mail")
```

This works all the same with the above definition of `myScratchpads`.

A full example of how a scratchpad setup would look using this machinery
can be found in [my config](https://gitlab.com/slotThe/dotfiles/-/blob/master/xmonad/.config/xmonad/src/xmonad.hs#L414).

## Calling Emacs in Scripts

Spawning frames is nice and all, but how about something more
complicated, like Emacs's batch mode so that we can use it properly in
scripts?  No problem at all!

For example, I have the following snippet in my config to get the
currently selected text and call [arxiv-citation] with it to [produce a
citation entry in my bibliography
files](/posts/phd-workflow/2022-05-01-my-phd-workflow.html#citations):

``` haskell
callArXiv :: String -> X ()
callArXiv fun = do
  url <- getSelection  -- from X.U.XSelection
  proc $ inEmacs
     >-> withEmacsLibs [ ElpaLib "dash", ElpaLib "s"
                       , ElpaLib "arxiv-citation"
                       , Special "~/.config/emacs/private-stuff.el" ]
     >-> asBatch
     >-> eval (progn [require "arxiv-citation", fun <> asString url])
```

When executed, this translates to something like

```
emacs -L /home/slot/.config/emacs/elpa/dash-20220417.2250
      -L /home/slot/.config/emacs/elpa/s-20210616.619
      -L /home/slot/.config/emacs/elpa/arxiv-citation-20220510.1137/
      --batch
      --eval '(progn
                (require (quote arxiv-citation))
                (arXiv-citation "<url-in-the-primary-selection>"))'
```

I certainly know which one I'd rather type—especially with ELPA
directory names changing quite frequently!  On that note,
[arxiv-citation] is on MELPA now; yay!

## Other Programs

As this is my main use case for it, the new features of
`XMonad.Util.Run` are quite specialised for Emacs.  However, even for
other programs they may well come in handy.  Drawing from the point
about scratchpads again, here is a hypothetical one that spawns a ghci
session:

``` haskell
  ghci <- proc $ inTerm >-> setXClass calcInstName >-> execute "ghci"
```

Further, something that's very useful when dealing with [topic-based
workspaces](/posts/phd-workflow/2022-05-01-my-phd-workflow.html#topics)
is spawning a terminal or an editor already in the current topic
directory:

``` haskell
import XMonad.Actions.TopicSpace  -- for currentTopicDir and more
topicConfig = …

spawnTermInTopic :: X ()
spawnTermInTopic =
  proc $ termInDir >-$ currentTopicDir topicConfig

-- Optionally, modify the path to the editor with a function.
spawnEditorInTopic :: (String -> String) -> X ()
spawnEditorInTopic with =
  proc $ inEditor >-$ with <$> currentTopicDir topicConfig
```

Quite convenient if you ask me.

If you have or know of a use case you would like to support but which is
awkward with the current set of functions and combinators do not
hesitate to open a pull request or an issue!

# Implementation Considerations

The implementation is actually very straightforward—no really, check out
the
[source](https://github.com/xmonad/xmonad-contrib/blob/master/XMonad/Util/Run.hs#L303)
if you don't believe me!

One concept that's still worth touching upon is the internal use of
[difference list]s.  The basic idea of these things is that, instead of
concatenating strings one by one, we create functions `String -> String`
and then use function composition to do the work for us:

``` haskell
  -- Ordinary string concatenation
  "string1" <> "string2" <> "string3" <> "string4"

  -- Using difference lists:
  string1, string2, string3, string4 :: String -> String
  string1 s = "string1" <> s
  string2 s = …

  string1 . string2 . string3 . string4 $ ""
```

Note how we have to apply the entire thing to `""` at the end in order
to actually get a string back.  As a concrete example, assuming we have
set `"Emacs"` as our editor, the `inEditor` function would essentially
be

``` haskell
inEditor :: String -> String
inEditor s = " Emacs " <> s
```

There are some further considerations to be made, since we are in the
`X` monad and thus the type is actually `X (String -> String)` instead
of just `String -> String`, but that isn't too important for us here.

Difference lists have some performance advantages over the traditional
concatenation of strings.  The concatenation `(<>)` on strings is left
associative by default and so

``` haskell
    "string1" <> "string2" <> "string3" <> "string4"
  ≡ (("string1" <> "string2") <> "string3") <> "string4"
```

However, the complexity characteristics of this operation are working
against us here; the definition of `(<>)` on `String`[^1] is

``` haskell
(<>) :: String -> String -> String
[]       <> ys =           ys
(x : xs) <> ys = x : xs <> ys
```

We are merely traversing the first string, leaving the second one
completely untouched (and unevaluated!).  All in all, this means that
`s₁ <> s₂` is in `𝓞(|s₁|)`; given an expression of the form

``` haskell
  (("string1" <> "string2") <> "string3") <> "string4"
```

we will have to walk over `"string1"` three times!  What we actually
want is a right-associative ordering—exactly what function compositions
gives us.  Spelled out,

``` haskell
    string1 . string2 . string3 . string4 $ ""
  ≡ string1 (string2 (string3 (string4 "")))
  ≡ "string1" <> ("string2" <> ("string3" <> ("string4" <> "")))
```

which yields the desired behaviour.  In fact, this is so canonical that
instead of using `(.)`, we could have also—perhaps a bit
confusingly—used `(<>)` directly:

``` haskell
    string1  . string2  . string3  . string4
  ≡ string1 <> string2 <> string3 <> string4
```

This is the fact that the _endomorphisms_ for any type `a`—the functions
`a -> a`—form a _monoid_.  That is to say that they come equipped with
an associative an unital operation: function composition.  In Haskell,
`(<>)` is, in some sense,
[overloaded](https://www.haskell.org/tutorial/classes.html) so that it
can be used with any monoidal composition one can think of![^2]

The attentive reader may have concluded that the pipe operator that we
called `(>->)` above is really just `(<>)` in disguise, and that's
exactly right!  I, however, thought that for people not familiar with
Haskell, giving it a pipe-like appearance would be more conceptually
amenable to the threading idea.

I haven't benchmarked this, so it's not entirely clear to me whether
performance is actually relevant in this situation,[^3] but using
difference lists just feels right here, and so I did.

# Conclusion

I have to say that I'm quite satisfied with this API.  In fact, if I
look at the old code that only resided within my personal config, it's
even a bit more ergonomic in a few places, despite having essentially no
user-specific strings hardcoded anywhere!  As I said before, if you try
this and find something missing, do let me know and we'll probably find
a solution!  If you try this and find it useful, also let me know :)

Of course, technically none of this needs to live only inside your
XMonad config at all.  In combination with the excellent [turtle]
library, I reckon it would be quite easy to produce Haskell versions of
cool tools like magit.sh[^4].  Go nuts!

[EDSL]: https://en.wikipedia.org/wiki/Domain-specific_language
[arxiv-citation]: https://github.com/slotthe/arxiv-citation
[class or instance name]: https://tronche.com/gui/x/icccm/sec-4.html#WM_CLASS
[difference list]: https://github.com/spl/dlist#references
[magit.sh]: https://github.com/alphapapa/magit.sh
[research workflow]: /posts/phd-workflow/2022-05-01-my-phd-workflow.html#citations
[scratchpad]: https://hackage.haskell.org/package/xmonad-contrib/docs/XMonad-Util-NamedScratchpad.html
[turtle]: https://hackage.haskell.org/package/turtle
[xmonad-contrib]: https://github.com/xmonad/xmonad-contrib

[^1]: Really, this is the definition of `(++)` for arbitrary lists `[a]`
      and `(<>) = (++)` for `String = [Char]`, but let's not get into
      that here.

[^2]: Really, for any _semigroup_, which is a slightly weaker notion of
      an operation that is merely associative, but doesn't necessarily
      have a unit.

[^3]: I suspect that the answer is "probably not"—that didn't stop me,
      however!

[^4]: Available [here](https://github.com/alphapapa/magit.sh).  I also
      maintain a slightly modified and POSIX shell compatible version
      [here](https://gitlab.com/slotThe/dotfiles/-/blob/master/scripts/.scripts/magit.sh).

[^5]: If you _really_ want to try this feature but don't want to bother
      installing any unreleased—though stable—version, message me in any
      way and maybe we'll hurry up and cut 0.17.1 soon!
