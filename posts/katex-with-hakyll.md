---
title: Integrating KaTeX Into Hakyll
date: 2024-07-20
tags: haskell
---

<p></p>
Being quite into mathematics,
I sometimes blog about it.[^2]
There are very capable solutions for rendering LaTeX in HTML documents out there,
which in particular solve the problem of properly aligning the fragments with the rest of the text.
One of them is KaTeX,
advertising itself to be easily executed on the server-side,
avoiding the use of extraneous client-side JavaScript.
Integrating it with Hakyll turned out to be relatively straightforward,
yet I haven't seen an actual implementation anywhere;
this post is supposed to fill that gap.

<!--more-->

# My dark MathJax past

One of my quite strongly held opinions is that,
for static websites such as this one,
client-side LaTeX rendering is completely unnecessary,
and actually just a waste of resources.
As a result, I've been using MathJax
to insert LaTeX fragments into the HTML after it's compiled from Markdown.
This setup<!--
-->—stolen essentially verbatim from [Gwern][gwern:site]—<!--
-->uses the now deprecated [mathjax-node-page][mathjax-node-page]
to crawl through the already rendered HTML pages, and, upon recognising a math fragment,
replaces that text with the rendered formula.
The call to `mathjax-node-page` is trivial to parallelise on a per-file level with something like [GNU parallel],
and so the whole thing actually works quite well.

However, the fact that this is "external" to Pandoc's pipeline
and requires a separate `build.sh` file to be created has always felt a bit awkward to me.[^4]
Plus, Hakyll is already capable of using GHC's parallel runtime<!--
-->—why outsource a part of that to an external tool?
At some point, the annoyance I felt at this became stronger than the inertia my old setup had, so here we are.

# A brighter future with KaTeX

Naturally, when you change something you *really* want to change something<!--
-->—at least I do—<!--
-->so instead of using MathJax v3's native support for these kinds of things,
why not try something new?
An often cited alternative to MathJax is [KaTeX],
yet another JavaScript library that promises decent maths rendering on the web.
This one is pretty good, though;
it's supposed to be faster than MathJax,
and has "server side rendering" as a big bullet point on its landing page.
Sounds exactly like what I'm looking for.

KaTeX has a CLI of the same name,
but booting up the node runtime for every single maths fragment sounds perfectly dreadful to me,
so let's not do that.
As such, one probably can't avoid writing at least a little bit of JavaScript.
Thankfully, integrating KaTeX into Pandoc itself seems to be a well-trodden path,
so other people have already done this for me.
For example,
[pandoc#6651][pandoc:katex-integration]
has a tiny script—essentially just calling `katex.​render​To​String`—that
is fed maths on stdin,
and then produces HTML on stdout.
Slightly adjusted to support inline and display maths, it looks like this:

``` typescript
import { readLines } from "https://deno.land/std@0.224.0/io/mod.ts";
import katex from "https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.mjs";

for await (const line of readLines(Deno.stdin)) {
  try {
    let DISPLAY    = ":DISPLAY ";
    let useDisplay = line.startsWith(DISPLAY);
    let cleanLine  = useDisplay ? line.substring(DISPLAY.length) : line;
    console.log(katex.renderToString(cleanLine, {
      displayMode: useDisplay,
      strict: "error",
      throwOnError: true,
    }));
  } catch (error) {
    throw new Error(`Input: ${line}\n\nError: ${error}`);
  }
}
```

Having this in place,
all that's left is to crawl through Pandoc's AST,
and feed each maths fragment to KaTeX.
Transforming its AST is something that Pandoc does
[very][post:pandoc:pygments] [well][post:pandoc:bib],
so the code is usually swiftly written.
Indeed, both the [Block][pandoc:block] and [Inline][pandoc:inline] types
have a `Math` constructor which we can match on.[^3]

``` haskell
import Data.Text    qualified as T
import Data.Text.IO qualified as T
import GHC.IO.Handle (BufferMode (NoBuffering), Handle, hSetBuffering)
import Hakyll
import System.Process (runInteractiveCommand)
import Text.Pandoc.Definition (Block (..), Inline (..), MathType (..), Pandoc)
import Text.Pandoc.Walk (walk, walkM)

hlKaTeX :: Pandoc -> Compiler Pandoc
hlKaTeX pandoc = recompilingUnsafeCompiler do
  (hin, hout, _, _) <- runInteractiveCommand "deno run scripts/math.ts"
  hSetBuffering hin  NoBuffering
  hSetBuffering hout NoBuffering

  (`walkM` pandoc) \case
    Math mathType (T.unwords . T.lines . T.strip -> text) -> do
      let math :: Text
            = foldl' (\str (repl, with) -> T.replace repl with str)
                     case mathType of
                       DisplayMath{-s-} -> ":DISPLAY " <> text
                       InlineMath{-s-}  ->                text
                     macros
      T.hPutStrLn hin math
      RawInline "html" <$> getResponse hout
    block -> pure block
 where
  -- KaTeX might sent the input back as multiple lines if it involves a
  -- matrix of coordinates. The big assumption here is that it does so only
  -- when matrices—or other such constructs—are involved, and not when it
  -- sends back "normal" HTML.
  getResponse :: Handle -> IO Text
  getResponse handle = go ""
   where
    go :: Text -> IO Text
    go !str = do
      more <- (str <>) <$> T.hGetLine handle
      if ">" `T.isSuffixOf` more  -- end of HTML snippet
      then pure more
      else go   more

  -- I know that one could supply macros to KaTeX directly, but where is the
  -- fun in that‽
  macros :: [(Text, Text)]
  macros =
    [ ("≔"       , "\\mathrel{\\vcenter{:}}=")
    , ("\\defeq" , "\\mathrel{\\vcenter{:}}=")
    , ("\\to"    , "\\longrightarrow")
    , ("\\mapsto", "\\longmapsto")
    , ("\\cat"   , "\\mathcal")
    , ("\\kVect" , "\\mathsf{Vect}_{\\mathtt{k}}")
    ]
```

The `(T.unwords . T.lines . T.strip -> text)`
[view pattern][GHC:ViewPatterns]
is because KaTeX *really* does not seem to like it when there is a line break<!--
-->—even a semantically irrelevant one—<!--
-->in a formula.
Perhaps this is a setting I've overlooked.
Other than that the code should be reasonably self-explanatory;
there are a few macro definitions that are copied from the now deleted
[build.sh][site:old:build.sh]
and some fiddling to make the stdout handle actually output the full response.[^1]

The `hlKaTeX` function,
having a `Pandoc -> Compiler Pandoc` signature,
can be given to [pandocCompilerWithTransformM][] like any other function:

``` haskell
myPandocCompiler :: Compiler (Item String)
myPandocCompiler =
  pandocCompilerWithTransformM
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
    hlKaTeX
```

And that's pretty much it!

## Adding CSS

All that's left is to include the custom CSS and special fonts that KaTeX relies upon.
The former can be downloaded from [their CDN](https://cdn.jsdelivr.net/npm/katex@latest/dist/katex.min.css),
and the latter are easily obtained from
[the latest release](https://github.com/KaTeX/KaTeX/releases/)
by copying the `fonts` directory.
The fonts are both reasonably small and loaded on demand,
such that the website does not blow up in size with this switch.

# Conclusion

The whole affair was much easier than I<!--
-->—not knowing any JavaScript—<!--
-->expected it to be, and actually turned out to be quite fun.
Of course, nothing at all has changed on the user-side of things,
which is to say that the new KaTeX fragments look pretty much exactly the same as the old MathJax maths.
Still, the warm feeling I had when deleting that `build.sh` shell script tells me that this was not solely an exercise in futility.
Or perhaps I've fully embraced rolling the boulder up the hill by now.

If you're interested,
the commit adding it to my setup can be found
[here](https://github.com/slotThe/slotThe.github.io/commit/6114e0e2a568122c01236dee38e2bf772efbf1e5).

[GNU parallel]: https://www.gnu.org/software/parallel/
[KaTeX]: https://katex.org/
[gwern:site]: https://github.com/gwern/gwern.net/blob/c3d90bf1d6248d5e80dc030304c72b3f4a234455/build/sync.sh#L421
[mathjax-node-page]: https://github.com/pkra/mathjax-node-page
[pandoc:katex-integration]: https://github.com/jgm/pandoc/issues/6651
[post:pandoc:bib]: ./hakyll-and-bibtex.html
[post:pandoc:pygments]: ./pygmentising-hakyll.html
[pandoc:inline]: https://hackage.haskell.org/package/pandoc-types/docs/Text-Pandoc-Definition.html#t:Inline
[pandoc:block]: https://hackage.haskell.org/package/pandoc-types/docs/Text-Pandoc-Definition.html#t:Block
[GHC:ViewPatterns]: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/view_patterns.html
[site:old:build.sh]: https://github.com/slotThe/slotThe.github.io/blob/6cd02aab18e63a42a4b889e3cf4bba951277cad4/build.sh#L15
[pandocCompilerWithTransformM]: https://hackage.haskell.org/package/hakyll/docs/Hakyll-Web-Pandoc.html#v:pandocCompilerWithTransformM
[post:anki]: ./anki-latex.html
[post:phd-workflow]: ./my-phd-workflow.html

[^1]: Seemingly as always when subprocesses are involved,
      the hardest thing is to actually get all of the incantations right
      such that buffering does not deadlock your program indefinitely.

[^2]: Not as much as I should,
      I guess,
      but nowadays when I write maths it feels like a waste to not have it go into either
      [Anki][post:anki], [Org Roam][post:phd-workflow], or a paper,
      and these notes are not necessarily written/ready for public consumption.
      Oh well.

[^3]: {-} 󠀠

      󠀠

      󠀠

      󠀠

      󠀠

      󠀠

      󠀠

      󠀠

      󠀠

      Mind the `BlockArguments`—and the "s".

[^4]: Especially because, unlike in Gwern's case, this site is not super complex to build;
      there aren't any other moving parts that would require me to leave Haskell.
