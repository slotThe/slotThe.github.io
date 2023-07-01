---
title: "XMonad Module Showcase: X.A.Search"
date: 2023-03-19
tags: haskell, xmonad
---

I'd like to highlight another XMonad module that has become quite
essential to me: [XMonad.Actions.Search].  Its premise is simple: you
enter some text into the XMonad prompt, and it queries a search engine
of your choice with this input—straightforward, and yet very effective.

<!--more-->

In fact, this sounds so simple that one might immediately ask: what's
the point?  The problem with regular searching is that it's *slow*; who
has the time to open their browser, navigate to the relevant website,
and only then enter their search?  In today's world, where many things
now have their own dedicated search engines[^4], searching efficiently
becomes increasingly important.  Plus, the lower barrier to entry means
that looking something up—always a good idea, with all of humanities
collected knowledge at one's fingertips—may well become instinctive,
instead of feeling like a chore.

[XMonad.Actions.Search]: https://hackage.haskell.org/package/xmonad-contrib/docs/XMonad-Actions-Search.html

# Functionality

The basic workflow suggested by the module simply consists of picking a
search engine, and binding it to a key via `promptSearch`—that's it.
Additionally, there is also `selectSearch` to look up the current
(primary) selection.  For example, suppose we had bound[^1]

``` haskell
searchKeys :: [(String, X ())]
searchKeys =              -- def is an optional prompt config
  [ ("M-s"    , promptSearch def wikipedia)
  , ("M-u M-s", selectSearch     wikipedia)
  ]
```

This would enable us to search through Wikipedia normally by pressing
`M-s`, and directly look up the currently selected text with `M-u
M-s`.[^2] The whole things looks like this:

<p>
  <video width="100%" controls>
    <source src="../images/search-showcase/search-simple.webm"
              type="video/webm">
    Searching for "Alexander Grothendieck" once manually,
    and then highlighting that phrase and using the selection search.
  </video>
</p>

Further, there is a third workflow that is quite convenient: by default,
pressing `C-y` in the prompt inserts the current selection, ready for
further editing.

<p>
  <video width="100%" controls>
    <source src="../images/search-showcase/search-simple-alt.webm"
              type="video/webm">
    Showcasing the functionality described in the last paragraph.
  </video>
</p>

## Adding new engines

There are quite a few search engines built-in: at the time of writing,
X.A.Search sports about 40 different ones.  Thankfully, adding new
engines is easy, even in one's own configuration; e.g., to look up
something on [Hoogle](https://hoogle.haskell.org/), one would simply
define

``` haskell
hoogle :: SearchEngine
hoogle = searchEngine "hoogle" "https://hoogle.haskell.org/?hoogle="
```

This means that even if the module does not have a search engine you
want, it's trivial to define it yourself—and don't forget to upstream it
afterwards!

There also is the `searchEngineF` function, which instead of just a
string takes a function to also modify that string.[^5] With this, one
can build more complicated search engines; for example, the following
constitutes a very basic URL entry function, which may also work as a
sort of lightweight bookmarking system:

``` haskell
url :: SearchEngine
url = searchEngineF "url" \s ->
  let url = "https://"
   in if url `isPrefixOf` s then s else url <> s
```

The whole API is quite flexible.  In my personal configuration I have
defined a `git` search engine to quickly jump to certain projects on
several different forges:

``` haskell
git :: SearchEngine
git = searchEngineF "repo" \s -> if
  | s `elem` ["change-env", "irc-bot"]
    -> "https://gitlab.com/slotThe/" <> s
  | s `elem` ["kbd-mode", "kmonad"]
    -> "https://github.com/kmonad/" <> s
  | s `elem` [ "x11", "x11-xft", "xmonad", "xmonad-contrib"
             , "xmonad-docs", "xmonad-web" ]
    -> "https://github.com/xmonad/" <> s
  | s `elem` [ "vc-use-package", "arXiv-citation", "hmenu"
             , "slotThe.github.io", "query-replace-many" ]
    -> "https://github.com/slotThe/" <> s
  | s == "slotThe"    -> "https://github.com/slotThe/"
  | s == "void-linux" -> "https://github.com/void-linux/void-packages"
  | s == "xmobar"     -> "https://codeberg.org/xmobar/xmobar"
  | otherwise         -> ""
```

It works as expected—so much so that I can enable the prompt's
auto-complete feature, which automatically completes a query if there is
only a single candidate left.[^6]

<p>
  <video width="100%" controls>
    <source src="../images/search-showcase/search-git.webm"
            type="video/webm">
            <img class="pure-img"
    Showcasing the git search engine defined above
  </video>
</p>

## Further tweaks

As I said, the module is pretty flexible with how exactly one can use
it, and so here are a few more personal tweaks that I figure might also
be of interest to others.  First, my setup is integrated with
[XMonad.​Actions.​Prefix]—I use the prefix argument to decide whether I
want "normal" searches, or ones using the primary selection.

``` haskell
-- | Search commands; integration with X.A.Prefix.
searchKeys :: Keybindings
searchKeys =
  [ ("M-s", withPrefixArgument $ submap . searchEngineMap . \case
      Raw _ -> -- normal searches
        selectSearchBrowser
      _     -> -- use the primary selection
        \br se -> promptSearchBrowser' (decidePrompt se) br se)
  ]
```

This is nice because `searchKeys` is now the only two place where this
distinction has to be made; others functions, like `search​Engine​Map`, do
not need to differentiate between a select search and a prompt search.
What I do separately match on is (i) the type of browser that I want to
open the searches in, and (ii) the type of prompt I would like to use.
This is very dependent on the engine itself; some, like `git` as defined
above, greatly benefit from keeping a history, while in others this is
more of a hinderance than anything else.  The browser situation is
similar.  An except of the totality of `searchKeys` looks like the
following.[^7]

``` haskell
searchKeys :: Keybindings
searchKeys =
  [ ("M-s", withPrefixArgument $ submap . searchEngineMap . \case
      Raw _ -> selectSearchBrowser
      _     -> \br se -> promptSearchBrowser' (decidePrompt se) br se)
  ]
 where
  -- | Match on the prompt type; this needs an orphan 'Eq' instance
  -- for 'SearchEngine'.  @prompt@ is my prompt config.
  decidePrompt :: SearchEngine -> XPConfig
  decidePrompt se
    | se `elem` [arXiv, wikipedia, github] = promptNoHist-- no history
    | se `elem` [git] = prompt{ autoComplete = (5 `ms`) }
    | otherwise = prompt

  -- | Open searches, possibly in a new window.
  searchEngineMap :: (Browser -> SearchEngine -> X ())
                  -> Map (KeyMask, KeySym) (X ())
  searchEngineMap searchIn = basicSubmapFromList
    [ (xK_a, sw arXiv    )
    , (xK_w, nw wikipedia)
    , (xK_g, submap $ basicSubmapFromList  -- submaps in submaps
              [ (xK_g, sw' git)
              , (xK_h, sw' github)
              ])
    ]
   where
    -- | Same window, new window.
    sw, sw', nw :: SearchEngine -> X ()
    sw  = searchIn browser
    nw  = searchIn "browser-new-window.sh"
    sw' = searchIn altBrowser

instance Eq {- ORPHAN -} SearchEngine where
  (==) :: SearchEngine -> SearchEngine -> Bool
  (SearchEngine n _) == (SearchEngine n' _) = n == n'
```

For my full configuration, see [here][config:search].

# Conclusion

As always, even the simplest topic—that of a search—leaves a lot more
room for personalisation than one would initially think.  Also as
always, XMonad delivers on the "there is already a module for that"
front.

For me personally, X.A.Search has really alleviated this perceived
slowness in searching, especially when using many different engines
simultaneously.  So much so, in fact, that I no longer have a problem
looking up information from multiple sources mid (text) conversation.
This not only helps me, but I reckon a few users of the `#xmonad` [IRC
Channel][libera:webchat] are quite glad about this as well!

[XMonad.Util.EZConfig]: https://hackage.haskell.org/package/xmonad-contrib/docs/XMonad-Util-EZConfig.html
[XMonad.​Actions.​Prefix]: https://hackage.haskell.org/package/xmonad-contrib/docs/XMonad-Actions-Prefix.html
[config:search]: https://gitlab.com/slotThe/dotfiles/-/blob/c31bc324a8cc3df5449d9ca7a50aabfcd87a7ad8/xmonad/.config/xmonad/src/xmonad.hs#L720
[libera:webchat]: https://web.libera.chat/#xmonad

[^1]: {-} 󠀠

       󠀠

       This uses [XMonad.Util.EZConfig] syntax.

[^2]: As an aside, this is an ideal use case for [XMonad.​Actions.​Prefix].
      With that module, one could define an equivalent keybinding along the lines of

      ``` haskell
      searchKeys :: [(String, X ())]
      searchKeys =
        [ ("M-s", withPrefixArgument $ \case
            Raw _ -> selectSearch     wikipedia
            _     -> promptSearch def wikipedia)
        ]
      ```

      This is especially useful when multiple search engines are involved;
      see [below](#further-tweaks) or check out [my configuration][config:search] for a complete example.

[^4]: Just to name a few, I regularly look things up on
      [zbmath](https://zbmath.org/),
      [Hoogle](https://hoogle.haskell.org/),
      [ClojureDocs](https://clojuredocs.org/),
      [Wikipedia](https://en.wikipedia.org/),
      [Open&shy;Street&shy;Map](https://www.openstreetmap.org/),
      [searX](https://searx.github.io/searx/),
      [arXiv](https://arxiv.org/),
      [noogle](https://noogle.dev/),
      forges like [Git&shy;Hub](https://github.com/),
      … the list goes on.

[^5]: {-} For the following code examples, you may need to place extra
      pragmas along the lines of

      ``` haskell
      {-# LANGUAGE BlockArguments #-}
      {-# LANGUAGE LambdaCase     #-}
      {-# LANGUAGE MultiWayIf     #-}
      ```

      at the top of your XMonad configuration file.

[^6]: In this case here, that single candidate will be from the relevant
      prompt history.

[^7]: {-} Just for completeness, an in order to improve
    copy-pasteability, the definition of `basicSubmapFromList` used a
    lot in this piece of code is:

    ``` haskell
    basicSubmapFromList
      :: Ord key
      => [(key, action)]
      -> Map (KeyMask, key) action
    basicSubmapFromList =
      fromList . map \(k, a) -> ((0, k), a)
    ```

    This is just creates a basic (i.e. there is no additional 'KeyMask' to consider)
    submap from a list of `(key, action)` pairs.
