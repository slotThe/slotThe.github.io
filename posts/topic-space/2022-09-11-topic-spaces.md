---
title: "XMonad Module Showcase: X.A.TopicSpace"
tags: xmonad
---

One of my favourite—and most used—modules is XMonad.Actions.TopicSpace.
However, it seems relatively unknown among the general XMonad community.
I fear this is due to the fact that the module is quite old and formerly
had a rather high barrier to entry.  Despite having been given shiny
[new documentation][topic-space], lots of people probably did not bother
revisiting it and thus still don't really understand why they might be
interested in using topics instead of workspaces.  Time to change that!

<!--more-->

# Introduction

First of all, this post is not to be seen as a tutorial on
X.A.TopicSpace, but much rather as a showcase of how its functionality
could be used day to day.  If you like what you see, perhaps check out
the [docs][topic-space] and give it a spin yourself!  I have already
written an introduction to the module in the post about my [research
workflow]:

> XMonad has a module called TopicSpace, which upgrades the X11
> workspace—virtual desktop—concept to so-called topics.  These are
> workspaces with a "theme" associated to them; for example, I have a
> topic for every project that I'm currently working on.  This results
> in a very clean separation of concerns.  Plus, I always know where my
> windows are!
>
> Every topic has a directory and a "startup hook", firing when the
> topic is switched to and empty, associated to it.  While most
> convenient for programming related tasks—e.g., spawn `ghcid` in the
> relevant directory or automatically build and open this website—it's
> also quite convenient for mathematical projects.
>
> I have set up special keybindings to bring up an Emacs session in the
> topic directory, or spawn a terminal there.  Switching to topics is
> done fuzzily via the XMonad prompt, which means I only have to type a
> few characters to get to my destination.  This makes it feasible to
> have 30 topics, instead of the usual 9 or so, in the first place.  As
> a result, it's rather fast to go from thinking about a certain problem
> to working on it.

At a glance, this probably does not sound very impressive—so one can
have a directory and some function associated to a workspace (hereafter
also called "topic"), big deal.  However, we will see that with a bit of
creativity this can be used to great effect.

# Examples

## Basic topics

The most obvious application of all of this is to have workspaces that
do one and only one thing.  For example, I have a topic dedicated to
connecting to a VPN, should the need arise.  Naturally, I automatically
want to execute `openvpn` and pick a random server whenever I happen to
enter that workspace and it's empty (i.e., `openvpn` is not already
running).

More such use cases include having a topic dedicated to my RSS feed
reader, instant messaging, or IRC.  Since I only show workspaces with
windows on them in xmobar, I can just glance at my status bar to find
out whether I currently have, for example, IRC open.  No additional
program for checking the status of things necessary!  Obviously, this
_modus operandi_ takes a bit of discipline to uphold over the course of
the day, but I find that such a separation of concerns greatly reduces
mental load regarding what's currently happening on my computer.
Definitely worth it.

In terms of code, this—as well as the following examples—heavily use the
[new interface][xmonad.util.run] to XMonad.Util.Run, which allows one to
spawn processes in a declarative and compositional way; I've [written
about this][run-post] in another post.  For example, my RSS topic is
specified thusly:

``` haskell
-- import XMonad.Actions.TopicSpace (inHome)
-- import XMonad.Util.Run

  , inHome "7:RSS" $ proc $ inEditor
                        >-> setFrameName "elfeed"
                        >-> execute (elispFun "elfeed")
```

Here, `inHome` is a little helper function that takes a topic name and
an action, and creates a new topic with `$HOME` as its associated
directory.

You can find all of my topics (and there are a lot of them)
[here](https://gitlab.com/slotThe/dotfiles/-/blob/master/xmonad/.config/xmonad/src/xmonad.hs#L219-L265).

## Spawning *everything* in the topic directory

More generally, programming projects in the same language almost always
require me to open the same set of standard tools, so it's extremely
convenient to directly spawn them upon first visit.  This allows for
very little friction before starting to work on whatever I wanted to
work on.

For example, I want to open Emacs and [ghcid] in every Haskell project
of mine—so why not automate this?  Using what X.U.Run gives us, we can
quickly throw together a function that executes the given instruction
inside of a terminal:

``` haskell
-- import XMonad.Actions.TopicSpace (currentTopicDir)
-- 'topicConfig' is my personal topic configuration.

-- | Execute a program in the topic directory (inside a terminal).
executeInTopic :: String -> X ()
executeInTopic p = proc $ (termInDir >-$ currentTopicDir topicConfig)
                      >-> execute p
```

Similar functions can be created for spawning the terminal and editor:

``` haskell
-- Whatever you're looking for, it's probably in X.A.TopicSpace
-- or X.U.Run.

-- | Spawn terminal in topic directory.
spawnTermInTopic :: X ()
spawnTermInTopic = proc $ termInDir >-$ currentTopicDir topicConfig

-- | Spawn editor in the current topic directory.
spawnEditorInTopic :: X ()
spawnEditorInTopic = proc $ inEditor >-$ currentTopicDir topicConfig
```

Check the documentation of [XMonad.Util.Run] to see how `inEditor` and
`termInDir` are defined and may be customised.

In my mathematical and other work-adjacent projects I keep it pretty
simple; an editor there is mostly sufficient.

<img class="pure-img" src="../phd-workflow/topics.gif">

But we can also get a little bit more fancy.  Since the topic action is
just a general `X` action, we can really do anything we want in there.
In addition to spawning programs, all of my Haskell projects should
default to the `Hacking`[^1] layout:

``` haskell
spawnHaskell :: X ()
spawnHaskell = sendMessage (JumpToLayout "Hacking")
            *> spawnEditorInTopic
            *> executeInTopic "ghcid"
```

And Voilà, we can now attach this action to all the topics that we want!

Note that the `*>` operator is—in this case—just the sequencing of
actions.  If you're more comfortable with `do` notation, you can also
write the above as

``` haskell
spawnHaskell :: X ()
spawnHaskell = do
  sendMessage (JumpToLayout "Hacking")
  spawnEditorInTopic
  executeInTopic "ghcid"
```

Furthermore, since the associated directory for a topic can easily be
made `$HOME` by default (as we've seen, TopicSpace even exports the
`inHome` function), spawning programs in certain topics can easily be
made to replace the default keybindings!

For the sake of completeness, I will showcase one slightly more
complicated example.  My main shell environment is `eshell` and getting
sane behaviour there presents one with a few more obstacles than
`spawnTermInTopic` did.  It also uses `inProgram` instead of `inEditor`,
allowing access to a different instance of the Emacs server.

``` haskell
-- | Spawn an eshell frame in the current topic directory.
spawnEshellInTopic :: X ()
spawnEshellInTopic = currentTopicDir topicConfig >>= \dir ->
  proc $ inProgram "emacsclient -a '' -c -s eshell"
     >-> execute (progn [ "eshell" <> quote "new-shell"
                        , "eshell/cd" <> asString dir
                        , "eshell/clear-scrollback"
                        , "eshell-send-input"
                        ])
```

All in all, we have something that looks a little bit like this:

<img class="pure-img" src="./haskell-topic.gif">

## Testing this website

Much in the same vein as my Haskell topics, I find the `website` topic
to be extremely handy—you can probably guess what it's used for.  Its
associated function `spawnWebsite` switches to the "Tall" layout, spawns
an Emacs frame in the topic directory, builds the website, and opens a
browser window pointing to the local copy:

``` haskell
spawnWebsite :: X ()
spawnWebsite = switchToLayout "Tall"
            *> spawnEditorInTopic
            *> executeInTopic "hakyll-build.sh --hold"
            *> spawn "browser-new-window.sh localhost:8000"
```

The whole thing looks like this:

<img class="pure-img" src="./website.gif">

# Conclusion

Hopefully these examples have convinced you to give TopicSpace a spin;
perhaps you've even gotten some ideas of your own you'd like to try out.
Although conceptually very simple, the module can be used in a variety
of ways to automate boring tasks just that tiny bit more—definitely a
win in my book!

[XMonad.Util.Run]: https://hackage.haskell.org/package/xmonad-contrib/docs/XMonad-Util-Run.html#g:EDSL
[ghcid]: https://github.com/ndmitchell/ghcid
[research workflow]: ../phd-workflow/2022-05-01-my-phd-workflow.html
[run-post]: ../2022-05-25-calling-emacs-from-xmonad.html
[topic-space]: https://hackage.haskell.org/package/xmonad-contrib/docs/XMonad-Actions-TopicSpace.html

[^1]: In case you are interested:
``` haskell
    hacking = renamed [Replace "Hacking"]
            . limitWindows 3
            . magnify 1.3 (NoMaster 3) True
            $ ResizableTall 1 (3 % 100) (13 % 25) []
```
      As with the rest of my dotfiles, it's available [here](https://gitlab.com/slotThe/dotfiles/-/blob/master/xmonad/.config/xmonad/src/xmonad.hs#L341).
