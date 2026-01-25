---
title: Pygmentising Hakyll's Syntax Highlighting
date: 2023-01-21
last-modified: 2025-01-05
tags: haskell, hakyll
---

By default, Hakyll uses pandoc to generate syntax highlighting for all
kinds of different programming languages.  However, even in simple
examples the HTML this produces is unsatisfactory.  Thankfully, the two
programs are almost infinitely customisable, and changing pretty much
any setting doesn't usually involve a lot of work—this is no exception.
Using `pygmentize` as an example, I will show you how you can swap out
pandoc's native syntax highlighting with pretty much any third party
tool that can output HTML.

<!--more-->

# The problem

Pandoc uses the [skylighting] library to generate syntax highlighting
for a given block of code.  Skylighting, in turn, uses [KDE XML syntax
definitions] for the respective tokenisers.  However, even for simple
examples I don't agree with the HTML this generates.  Consider the
following Haskell code block.

``` haskell
fibs :: [Integer]
fibs = 0 : scanl' (+) 1 fibs
```

Pandoc would generate something like the following:

``` html
<div class="sourceCode" id="cb1">
  <pre class="sourceCode haskell">
    <code class="sourceCode haskell">
      <span id="cb1-1">
        <a href="#cb1-1" aria-hidden="true" tabindex="-1"> </a>
        <span class="ot">    fibs :: </span> [<span class="dt">Integer </span>]
      </span>
      <span id="cb1-2">
        <a href="#cb1-2" aria-hidden="true" tabindex="-1"> </a>    fibs
        <span class="ot">= </span>
        <span class="dv">0 </span> <span class="op">: </span>
        scanl&#39; (<span class="op">+ </span>) <span class="dv">1
        </span> fibs
      </span>
    </code>
  </pre>
</div>
```

One can already see a few things wrong with this: (i) in the type
signature, the name of the list is smushed together with the separating
double colon (worse: it's just in the "other" syntax class), (ii) in the
actual definition, `fibs` isn't assigned any class at all, and (iii) the
assignment operator is also in the "other" class, instead of something
related to it being a built in operator!  As one can imagine, this only
gets worse as snippets get more complicated.

These kinds of issues, combined with the fact that certain
languages—like Emacs Lisp—don't have any syntax definitions at all,
annoyed me enough to look for an alternative way to highlight code on
this website.[^1] There are of course many options to choose from; I
went with `pygmentize`, solely because I already had it installed.  All
that's left is to tell pandoc and Hakyll to make use of it.  As
mentioned, this thankfully doesn't turn out to be very difficult!

# Playing with `pygmentize`

Having never used `pygmentize` as a command line utility,[^2] I expected
this to take some work—possibly involving Python *shudder*—but all of
the necessary pieces are already present in the CLI.  First up, the `-f`
option specifies the formatter to use, which will decide the shape of
the output.

``` console
$ pygmentize -L formatter | grep html
* html:
    Format tokens as HTML 4 ``<span>`` tags within a ``<pre>`` tag, wrapped
    in a ``<div>`` tag. The ``<div>``'s CSS class can be set by the `cssclass`
    option. (filenames *.html, *.htm)
```

We can test how this highlighting looks straight away; executing

``` console
$ echo "fibs :: [Integer]\nfibs = 0 : scanl' (+) 1 fibs" \
   \ | pygmentize -l haskell -f html
```

produces an HTML output along the lines of

``` html
<div class="highlight">
  <pre>
    <span> </span>
    <span class="nf">fibs </span> <span class="w"> </span>
    <span class="ow">:: </span> <span class="w"> </span>
    <span class="p">[</span>
      <span class="kt">Integer </span> <span class="p">] </span>
    <span class="nf">fibs </span> <span class="w"> </span>
    <span class="ow">= </span> <span class="w"> </span>
    <span class="mi">0 </span> <span class="w"> </span>
    <span class="kt">: </span> <span class="w"> </span>
    <span class="n">scanl&#39; </span> <span class="w"> </span>
    <span class="p">(</span>
      <span class="o">+ </span> <span class="p">) </span>
    <span class="w"> </span>
      <span class="mi">1 </span> <span class="w"> </span>
    <span class="n">fibs </span>
  </pre>
</div>
```

This looks much better!  The class names are kind of obtuse, but
`pygmentize` can also give you nicely annotated CSS styles for its
supported colour schemes.  For example, the following is a small excerpt
of the output:

``` console
$ pygmentize -S emacs -f html
…
.nf { color: #00A000 }                    /* Name.Function */
.ow { color: #AA22FF; font-weight: bold } /* Operator.Word */
.kt { color: #00BB00; font-weight: bold } /* Keyword.Type */
.w { color: #bbbbbb }                     /* Text.Whitespace */
.c { color: #008800; font-style: italic } /* Comment */
…
```

You can redirect this into a `pygments.css` file, link to it (e.g., from
your `default.html` template), and be on your way.  The annotation also
makes it very easy to change that file after the fact, in case
`pygmentize` does not have the theme that you want.

# Integration

The idea of what we want to do is quite simple: for every code block in
a given post, shell out to `pygmentize`, and use its output to replace
the block, somehow making sure pandoc doesn't touch it afterwards.
Let's solve this step by step.

## Pandoc

Pandoc has an aptly named `Pandoc` type, which represents the internal
structure of a document.

``` haskell
data Pandoc = Pandoc Meta [Block]
```

We'll neglect the metadata for now and just look at the `Block`s;
specifically, we want to zoom in on two constructors that will give you
everything we need:

``` haskell
data Block
  -- Lots of other constructors omitted
  = CodeBlock Attr Text   -- ^ Code block (literal) with attributes
  | RawBlock Format Text  -- ^ Raw block

-- | Attributes: identifier, classes, key-value pairs
type Attr = (Text, [Text], [(Text, Text)])

-- | Formats for raw blocks
newtype Format = Format Text
```

To get a feeling for how these `CodeBlock`s look, again consider our
`fibs` example from above.  By default, the corresponding `CodeBlock`
for this would look something like

``` haskell
CodeBlock ("", ["haskell"], [])
          "fibs :: [Integer]\nfibs = 0 : scanl' (+) 1 fibs"
```

Importantly, the language (if any) is the first argument of the
`classes` field of `Attr`.

A strategy begins to form: look for all occurences of a `CodeBlock` in
the `Pandoc` type, and replace it with a `RawBlock "html"` such that it
isn't touched anymore.  Doing so will not pose very many
challenges—pandoc has really great capabilities for
[walking][pandoc:walk] its AST in order to facilitate exactly these
kinds of changes.  Unsurprisingly, the `Walkable` class resides over all
things walkable; an abbreviated definition looks like this:

``` haskell
class Walkable a b where
  -- | @walk f x@ walks the structure @x@ (bottom up) and replaces every
  -- occurrence of an @a@ with the result of applying @f@ to it.
  walk  :: (a -> a) -> b -> b
  walk f = runIdentity . walkM (return . f)

  -- | A monadic version of 'walk'.
  walkM :: (Monad m, Applicative m, Functor m) => (a -> m a) -> b -> m b
```

Specifically, as we'll need to shell out to an external program, let us
restrict our attention to the more general `walkM` function here.  There
is an instance

``` haskell
instance Walkable Block Pandoc
```

which will be all that we need.  The necessary code now just
materialises in front of our eyes:[^3]

``` haskell
-- {-# LANGUAGE BlockArguments    #-}
-- {-# LANGUAGE LambdaCase        #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE ViewPatterns      #-}
--
-- import Data.Maybe (fromMaybe, listToMaybe)
-- import qualified Data.Text as T
-- import Hakyll
-- import System.Process (readProcess)
-- import Text.Pandoc.Definition (Block (CodeBlock, RawBlock), Pandoc)
-- import Text.Pandoc.Walk (walk, walkM)

pygmentsHighlight :: Pandoc -> Compiler Pandoc
pygmentsHighlight = walkM \case
  CodeBlock (_, (T.unpack -> lang) : _, _) (T.unpack -> body) ->
    RawBlock "html" . T.pack <$> unsafeCompiler (callPygs lang body)
  block -> pure block
 where
  pygs :: String -> String -> IO String
  pygs lang = readProcess "pygmentize" ["-l", lang, "-f", "html"]
```

Notice how _a priori_ this would have type `Pandoc -> IO Pandoc`, but
since we want to use it from Hakyll I've already inserted a call to
`unsafeCompiler` in the correct place.

Further, the above code checks whether the block has an explicit
language attached to it and, if not, leaves it alone; this was suggested
by [LSLeary] on Reddit.  If you want to have a single `div` class for
every code block—say, for some custom CSS—then you can replace

``` haskell
  CodeBlock (_, (T.unpack -> lang) : _, _) (T.unpack -> body) ->
    RawBlock "html" . T.pack <$> unsafeCompiler (callPygs lang body)
```

with

``` haskell
  CodeBlock (_, listToMaybe -> mbLang, _) (T.unpack -> body) -> do
    let lang = T.unpack (fromMaybe "text" mbLang)
    RawBlock "html" . T.pack <$> unsafeCompiler (callPygs lang body)
```

## Hakyll

Thankfully, integrating `pygmentsHighlight` into Hakyll is not very
complicated either.  In addition to the normal `pandocCompiler` or
`pandocCompilerWith` functions that you are probably already using,
there is also [pandocCompilerWithTransformM]:

``` haskell
pandocCompilerWithTransformM
  :: ReaderOptions
  -> WriterOptions
  -> (Pandoc -> Compiler Pandoc)
  -> Compiler (Item String)
```

Basically, in additions to reader and writer options, it also takes a
monadic transformation of pandoc's AST and builds an appropriate
`Compiler` from that.

``` haskell
-- import Hakyll
-- import Text.Pandoc.Options

myPandocCompiler :: Compiler (Item String)
myPandocCompiler =
  pandocCompilerWithTransformM
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
    pygmentsHighlight
```

The `myPandocCompiler` function can now be used as any other compiler;
for example:

``` haskell
main :: IO ()
main = hakyll do
  -- …
  match "posts/**.md" do
    route (setExtension "html")
    compile $ myPandocCompiler
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls
  -- …
```

For a full working example, see [my configuration][cfg:site].

# Conclusion

That's it!  To my eyes, syntax highlighting looks much better now, and
on the way I—and perhaps you as well—even learned a little bit about how
pandoc internally represents its documents.  Time well spent.  As I said
in the beginning, in principle one could swap out `pygmentize` for any
other syntax highlighter that can produce HTML.  However, for me these
results are good enough that I will probably not try out every tool
under the sun, chasing that ever present epsilon of highlighting cases
which I still don't agree with—at least for now.

# Epilogue: a pygments server
<span style="position: relative; top: -0.8em">*Added on 2025-01-05*</span>

Shelling out to `pygmentize` every time is quite expensive,
and once the site contains its fair share of code blocks one is curious if better solutions exist.
As [pygments](https://github.com/pygments/pygments) itself is a Python project,
one can instead spin up the interpreter once, and then just query the API a whole lot:

::: {.include from="scripts/pygmentize.py"}
:::

To integrate this with the Haskell side of things,
one just has to faff around with a bit of bidirectional process communication.

``` haskell
pygmentsHighlight :: Pandoc -> Compiler Pandoc
pygmentsHighlight pandoc = unsafeCompiler do
  (hin, hout, _, _) <- runInteractiveCommand "python scripts/pygmentize.py"
  hSetBuffering hin  NoBuffering
  hSetBuffering hout NoBuffering
  (`walkM` pandoc) \case
    CodeBlock (_, listToMaybe -> mbLang, _) body -> do
      let lang = fromMaybe "text" mbLang
      T.hPutStr hin $ T.intercalate "\n" [lang, tshow (T.length body), body]
      RawBlock "html" <$> getResponse hout
    block -> pure block
 where
  getResponse :: Handle -> IO Text
  getResponse h = go []
   where
    go :: [Text] -> IO Text
    go !acc = do
      ln <- T.hGetLine h
      if "</div>" `T.isSuffixOf` ln
        then pure $ T.intercalate "\n" (reverse (ln : acc))
        else go (ln : acc)
```

# Backlinks

- Vaibhav Sagar has written a
  [fantastic post][backlinks:ghc-syntax-highlighter-hakyll]
  outlining how one can use GHC itself to generate highlighting for Haskell code
  using the [ghc-syntax-highlighter] library.
  Seeing how there are a lot of language extensions that `pygmentize` does not highlight correctly,
  this seems well worth it!

[KDE XML syntax definitions]: https://docs.kde.org/stable5/en/kate/katepart/highlight.html
[LSLeary]: https://old.reddit.com/r/haskell/comments/10ilrui/pygmentising_hakylls_syntax_highlighting/j5fih5h/
[backlinks:ghc-syntax-highlighter-hakyll]: https://vaibhavsagar.com/blog/2023/01/29/ghc-syntax-hakyll/
[cfg:site]: https://github.com/slotThe/slotThe.github.io/blob/main/src/site.hs#L87
[ghc-syntax-highlighter]: https://hackage.haskell.org/package/ghc-syntax-highlighter
[minted]: https://ctan.org/pkg/minted?lang=en
[pandoc:walk]: https://hackage.haskell.org/package/pandoc-types/docs/Text-Pandoc-Walk.html
[pandocCompilerWithTransformM]: https://hackage.haskell.org/package/hakyll-4.15.1.1/docs/Hakyll-Web-Pandoc.html#v:pandocCompilerWithTransformM
[rewriting-the-technical-interview]: https://aphyr.com/posts/353-rewriting-the-technical-interview
[skylighting]: https://hackage.haskell.org/package/skylighting

[^1]: All of this work for a mostly greyscale theme!

[^2]: So far, the only interaction I had with the program was through
      the excellent [minted] LaTeX package.

[^3]: If all else fails, simply [trace the sigils in the air and give
      them form][rewriting-the-technical-interview].
