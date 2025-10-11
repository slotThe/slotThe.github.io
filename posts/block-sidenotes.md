---
title: Using Sidenotes with Hakyll
date: 2023-01-27
last-modified: 2024-07-17
tags: haskell, hakyll
---

I've become quite enamoured with sidenotes recently, and so of course
this website now has them as well!  Thankfully, the integration with
pandoc and Hakyll is quite straightforward, because other people have
already done the hard work.[^9]  Depending on your use-case, however, the
existing libraries might not entirely fit the bill; for example, by
default blocks that are more complicated than just paragraphs of pure
text don't work.  In this post, I'd like to explain an alternative
approach of integrating sidenotes into pandoc, which does enable the use
of these features.

<!--more-->

# Introduction

If you don't know: sidenotes are footnotes, just on the side of the
page![^1] More specifically they are footnotes placed inside of the
margins, in order to avoid having to click or scroll, breaking the flow
of reading—a flick of the eyes is enough.  This is very convenient,
especially for longer entries with lots of asides that don't necessarily
fit the flow of the article.  However, because websites are dynamic in
size, a fallback option should be provided in case the margins are too
small/nonexistent.  In most cases, this amount to making sidenotes
clickable again and, in one way or another, showing their content once
clicked.

Gwern Branwen has written about many different sidenote implementations
[here][gwern:sidenotes].  I settled on [Tufte CSS][github:tufte-css],
mainly because it seemed to be the most popular non-JS solution.  Plus,
there is the fantastic [pandoc-sidenote][hackage:pandoc-sidenote], which
provides an appropriate pandoc filter.  After extracting the relevant
CSS into [sidenotes.css][site:sidenotes] and plugging the exported
`usingSidenotes` function into my pandoc compiler,[^2] things just
worked!

There is but one problem with this setup: paragraphs.  A sidenote
roughly looks like the following:

``` html
<span class="sidenote-wrapper">
  <label for="sn-NAME" class="margin-toggle sidenote-number"> </label>
  <input type="checkbox" id="sn-NAME" class="margin-toggle">
  <span class="sidenote">
    SIDENOTEn
  </span>
</span>
```

As Chris MacKay—a Tufte CSS contributor—helpfully explains in an
[issue][github:tufte-css:spans], spans officially don't play nice with
paragraphs:

> so marginnotes and sidenotes, as they are implemented now, are
> `<span>` elements inside paragraph `<p>` elements.  Only inline
> elements are allowed inside paragraphs per the HTML standard.

I know what you're thinking: people don't usually let themselves be
hampered by standards, do they?[^4] Alas, the `pandoc-sidenote` package
quite sensibly converts a given footnote into an approximation of the
above HTML by using pandoc's native `Span` data constructor.  Here is a
(heavily) simplified version of the relevant function.

``` haskell
filterInline :: Inline -> Inline
filterInline = \case
  -- A @Note@ signifies a footnote.
  Note blocks ->
    let -- Note has a [Block], but Span needs [Inline]
        content = coerceToInline blocks
     in Span ("", ["sidenote-wrapper"], [])
             [ RawInline "html" "<label for …"
             , RawInline "html" "<input type …"
             , Span ("", ["sidenote"], []) content
             ]
  inline -> inline
```

Now, pandoc very much does _not_ want you to put any kind of block
inside of its `Span`s—that's why its type is

``` haskell
Span :: Attr -> [Inline] -> Inline
```

Notice the absence of a `[Block]` argument.  This is why
`coerceToInline` exists, which tries to convert as many blocks as it can
to inline elements.

``` haskell
coerceToInline :: [Block] -> [Inline]
coerceToInline = concatMap deBlock . walk deNote
 where
  deBlock :: Block -> [Inline]
  deBlock (Plain     ls    ) = ls
  -- Simulate paragraphs with double LineBreak
  deBlock (Para      ls    ) = ls ++ newline
  -- See extension: line_blocks
  deBlock (LineBlock lss   ) = intercalate [LineBreak] lss ++ newline
  -- Pretend RawBlock is RawInline (might not work!)
  -- Consider: raw <div> now inside RawInline... what happens?
  deBlock (RawBlock fmt str) = [RawInline fmt str]
  -- lists, blockquotes, headers, hrs, and tables are all omitted
  -- Think they shouldn't be? I'm open to sensible PR's.
  deBlock _                  = []

  deNote :: Inline -> Inline
  deNote (Note _) = Str ""
  deNote x        = x
```

The `coerceToInline` function behaves sensibly with respect to the
simplest kinds of blocks, but already mentions that the `RawBlock
→ RawInline` transformation may have some caveats.  For example,
[prerendering code blocks][blog:pygmentising] puts one in exactly such a
"now we have a `<div>` tag inside of a `RawInline` element" situation.
Well, what happens?
```{=html}
<span class="sidenote-wrapper">
<label for="sn-test" class="margin-toggle sidenote-number"></label>
<input type="checkbox" id="sn-test" class="margin-toggle">
<p><span class="sidenote">Now simulating <code>pandoc-sidenode</code>s behaviour, the following is a piece of code <em>in the sidenote:</em>
<div class="highlight"><pre><span></span><span class="nf">a</span><span class="w"> </span><span class="ow">::</span><span class="w"> </span><span class="kt">Int</span> <span class="nf">
a</span><span class="w"> </span><span class="ow">=</span><span class="w"> </span><span class="mi">42</span> </pre></div>
</span></p>
</span>
```

This is the next line in the main document; the above code block was
supposed to be in the relevant sidenote, but "spilled" into the text
instead.  This is obviously not what we want.

The documentation mentions other things that are missing, and that pull
requests are welcome, but for many blocks there just is no good
alternative.  For example, code blocks and inline code serve very
different purposes most of the time.  Pandoc itself has a
[function][hackage:pandoc:blocks-to-inlines] of the same name that
attempts to convert tables, figures, and the like, but this also yields
some surprising behaviour when used instead of `pandoc-sidenote`s
variant.[^3]

So what to do?  Well, life wouldn't be fun if we didn't at least try to
hack our way around a standard, would it?

[github:tufte-css:spans]: https://github.com/edwardtufte/tufte-css/issues/93#issuecomment-234316022
[github:tufte-css]: https://github.com/edwardtufte/tufte-css
[gwern:sidenotes]: https://gwern.net/Sidenotes
[hackage:pandoc-sidenote]: https://hackage.haskell.org/package/pandoc-sidenote
[hackage:pandoc:blocks-to-inlines]: https://hackage.haskell.org/package/pandoc-3.0.1/docs/Text-Pandoc-Shared.html#v:blocksToInlines
[site:sidenotes]: https://github.com/slotThe/slotThe.github.io/blob/main/css/sidenotes.css

# Rewriting `pandoc-sidenote`

As I've learned while [writing][blog:pygmentising] about using
`pygmentize` to syntax highlight code for this site, pandoc has quite
good support for changing its AST in creative ways.  A strategy unfolds:
find every `Note` block in a document, somehow render its contents, and
create a `RawBlock "html"` node instead of using pandoc's built in
`Span` .

For rendering HTML, pandoc has a
[writeHtml5String][hackage:pandoc:writeHtml5String] function, which is
conveniently wrapped by Hakyll in
[writePandocWith][hackage:hakyll:writePandocWith]:

``` haskell
-- | Write a document (as HTML) using pandoc, with the supplied options
writePandocWith :: WriterOptions  -- ^ Writer options for pandoc
                -> Item Pandoc    -- ^ Document to write
                -> Item String    -- ^ Resulting HTML
```

Importantly, rendering takes some `WriterOptions`; since we don't want
to mess around with changing pure HTML afterwards, this is quite
important for us.

Writing this filter now essentially works by the same strategy outlined
in the last post: look through the Haddocks to find the types that we
want, [seize some meaningless functions from the void, and imbue them
with meaning][aphyr:typing-the-technical-interview].  The relevant bits
from pandoc's internal types are the following.

``` haskell
data Pandoc = Pandoc Meta [Block]

data Block
    = Plain [Inline]       -- ^ Plain text, not a paragraph
    | Para [Inline]        -- ^ Paragraph
    -- …
    | RawBlock Format Text -- ^ Raw block

data Inline
    = Str Text             -- ^ Text (string)
    -- …
    | Note [Block]         -- ^ Footnote or endnote
```

While this document is _not_ a literate Haskell file, the following is
still here for convenience, in case you are left wondering where some of
the functions come from.

``` haskell
{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE StandaloneKindSignatures #-}

import Control.Monad.State (State, foldM, get, modify', runState)
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import Hakyll (Item (..), writePandocWith)
import Text.Pandoc.Definition (Block (..), Inline (..), Pandoc (..))
import Text.Pandoc.Options (WriterOptions)
import Text.Pandoc.Shared (tshow)
import Text.Pandoc.Walk (walkM)
```

Rendering the actual sidenote isn't very complicated, and amounts to
picking out the `Note` constructor, rendering it, and putting everything
back together.[^5]

``` haskell
renderSidenote :: [Inline]            -- ^ Inlines from a single @Note@
               -> Sidenote [Block]
renderSidenote = go []
 where
  go :: [Inline] -> [Inline] -> Sidenote [Block]
  go inlines = \case
    []           -> pure [Plain inlines]
    Note bs : xs -> do block <- renderBlock bs
                       ([Plain inlines, block] <>) <$> go [] xs
    b       : xs -> go (inlines <> [b]) xs

  renderBlock :: [Block] -> Sidenote Block
  renderBlock blocks = do
    -- Update sidenote counter and get the `WriterOption's.
    SNS w i <- get <* modify' (\sns -> sns{ counter = 1 + counter sns })
    pure . RawBlock "html" $ -- … all the opening html stuff
                             writePandocWith w (Item "" (Pandoc mempty blocks))
                             -- … all the closing html stuff
```

Finding notes is a bit more finicky, since they could potentially occur
in a lot of places.  Right now, for obvious reasons, I've settled on
covering all cases that currently occur on this website.  Importantly,
we need to be a bit careful about inserting newlines for paragraphs (and
when to omit this).  This is because `Note`s are actually inline
elements, and so we are replacing a single `Block` by a list of
`Block`s, which incurs some additional formatting.

``` haskell
mkSidenote :: [Block] -> Sidenote [Block]
mkSidenote = foldM (\acc b -> (acc <>) <$> single b) []
 where
  single :: Block -> Sidenote [Block]
  single = \case
    -- Simulate a paragraph by inserting a dummy block; this is needed
    -- in case two consecutive paragraphs have sidenotes, or a paragraph
    -- doesn't have one at all.
    Para inlines         -> (Para [Str ""] :) <$> renderSidenote inlines
    OrderedList attrs bs -> (:[]) . OrderedList attrs <$> traverse mkSidenote bs
    BulletList        bs -> (:[]) . BulletList        <$> traverse mkSidenote bs
    block                -> pure [block]
```

Putting everything together, we apply this transformation to every block
in a document:

``` haskell
usingSidenotes :: WriterOptions -> Pandoc -> Pandoc
usingSidenotes writer (Pandoc meta blocks) =
  Pandoc meta (walkBlocks (SNS writer 0) blocks)
 where
  walkBlocks :: SidenoteState -> [Block] -> [Block]
  walkBlocks sns = \case
    []       -> []
    (b : bs) -> b' <> walkBlocks s' bs
     where (b', s') = runState (walkM mkSidenote [b]) sns
```

This can now be used much like the `usingSidenotes` function from
`pandoc-sidenote`, only that it needs to know your `WriterOption`s.
More importantly, it should be the last of the transformations that you
do to pandoc's AST, since `usingSidenotes` completely renders the
footnote, which is not what you want in case you—like me—do other
creative transformations, such as separately generating HTML for
`CodeBlock`s.  In my configuration, I now have

``` haskell
myPandocCompiler :: Compiler (Item String)
myPandocCompiler =
  pandocCompilerWithTransformM
    defaultHakyllReaderOptions
    myWriter
    (pure . usingSidenotes myWriter <=< pygmentsHighlight  . addSectionLinks)
```

Tufte CSS needs to be changed minimally to support this extended
functionality, but thankfully Said Achmiz has already documented what
needs to be done [here][github:tufte-css:paragraph-fixes].  This is
already included in my `sidenotes.css`, so if you're just copying that
then you should be fine.

Even better, as Gwern Branwen [mentions][github:pandoc-sidenote:div], we
don't even need to use `span`s anymore!

> It's true that `Span` nodes are often unsuited for doing anything
> interesting involving a `Block` or `[Block]`, but that's precisely
> what `Div` is for: it's the `Block`-level equivalent of the `Inline`
> `Span`. And if you are rendering blocks into an HTML `<span>` wrapper,
> that would seem to be bad HTML practice:
> [`<span>`](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/span)
> is defined as being inline and containing inline stuff (which is why
> Pandoc makes it expressed as an `Inline`), in contrast to
> [`<div>`](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/div)
> block containers.

So far, I had assumed that there was a reason why Tufte CSS used `span`
elements instead of `div`s in their sidenote, but this does not seem to
be the case.[^7] Instead of writing

``` html
<span class="sidenote">
  «sidenote»
</span>
```

one may well use

``` html
<div class="sidenote">
  «sidenote»
</div>
```

instead—since we render directly to HTML now, that door opened for us.
This means no more standard violations!

[aphyr:typing-the-technical-interview]: https://aphyr.com/posts/342-typing-the-technical-interview
[blog:pygmentising]: https://tony-zorman.com/posts/pygmentising-hakyll.html
[github:pandoc-sidenote:div]: https://github.com/jez/pandoc-sidenote/issues/4#issuecomment-1426123545
[github:tufte-css:paragraph-fixes]: https://github.com/edwardtufte/tufte-css/issues/93#issuecomment-671102819
[hackage:hakyll:writePandocWith]: https://hackage.haskell.org/package/hakyll-4.15.1.1/docs/Hakyll-Web-Pandoc.html#v:writePandocWith
[hackage:pandoc:writeHtml5String]: https://hackage.haskell.org/package/pandoc-3.0.1/docs/Text-Pandoc-Writers.html#v:writeHtml5String

# Conclusion

Being perfectly honest, I'm not very satisfied with the module.
It does work, but pre-rendering everything and not using pandoc's built in constructors feels like a big hack.

However, this solution seems to be quite comfortable from a Hakyll
perspective,[^8] and people have told me that the functionality provided
is still useful to them.  Since what I've done here amounts to a
complete rewrite—and almost certainly a regression in places—I don't
think that this will ever replace `Text.​Pandoc.​SideNote` wholesale.
However, the `pandoc-sidenote` library now features a
[Text.​Pandoc.​SideNoteHTML][github:pandoc-sidenote:HTML]
module as an alternative.
This keeps backwards compatibility,
yet still allows people to make use of the alternative filter.

[github:pandoc-sidenote:custom-writer]: https://github.com/jez/pandoc-sidenote/issues/4#issuecomment-269744553
[site:sidenotes-hs]: https://github.com/slotThe/slotThe.github.io/blob/main/src/Sidenote.hs
[github:pandoc-sidenote:HTML]: https://github.com/jez/pandoc-sidenote/blob/master/src/Text/Pandoc/SideNoteHTML.hs

[^1]: For example, this is one.

[^2]: Just like you would expect:

      ``` haskell
      myPandocCompiler :: Compiler (Item String)
      myPandocCompiler =
        pandocCompilerWithTransformM
          defaultHakyllReaderOptions
          myWriter
          ( pygmentsHighlight -- syntax highlight
          . usingSidenotes    -- sidenotes
          . addSectionLinks   -- link on hover
          )
      ```

[^3]: Whereas now, things like

      - tables

        +---------------+---------------+--------------------+
        | Fruit         | Price         | Advantages         |
        +===============+===============+====================+
        | Oranges       | $2.10         | - cures scurvy     |
        |               |               | - tasty            |
        +---------------+---------------+--------------------+

      - quotes

        > they said that …

      - display maths

        $$
        \int^{a, b} \mathcal{C}(a \otimes b, {-}) \cdot Fa \otimes Gb
        $$

      and the like are no problem at all!

[^4]: Just ask any person who's ever worked on a window manager what
      creative "interpretations" programs have of [ICCCM] or [EWMH] :)

      [ICCCM]: https://x.org/releases/X11R7.6/doc/xorg-docs/specs/ICCCM/icccm.html
      [EWMH]: https://en.wikipedia.org/wiki/Extended_Window_Manager_Hints

[^5]: The `Sidenote` type you are seeing is just some alias for `State`,
      which keeps track of the sidenote number, as well as the supplied
      `WriterOption`s; nothing fancy.

[^6]: Not only that, but it's almost certainly also a regression in
      certain aspects.

[^7]: At least not to my current knowledge.

[^8]: Jake Zimmerman—the author of `pandoc-sidenote`—also
      [thought about this issue][github:pandoc-sidenote:custom-writer],
      and concluded that perhaps a custom pandoc `Writer` would be the
      most elegant solution to this problem.  However, since using
      pandoc's HTML5 writer is quite hard-coded into Hakyll, I don't think
      that I will go down that path.

[^9]: {-} To put this front and centre:
      the `pandoc-sidenote` library now features a
      [Text.​Pandoc.​SideNoteHTML][github:pandoc-sidenote:HTML]
      module, which is an implementation of what's discussed in this post!
