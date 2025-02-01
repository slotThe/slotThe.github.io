---
title: Incorporating BibTeX into Hakyll
date: 2023-06-20
tags: haskell, hakyll
---

When writing a blog post that feels academic—or pretentious—enough to invoke the need for citations,
having them automatically generated feels like a mandatory requirement.
I can only shudder to imagine the alternatives.

The good news is that LaTeX has solved this problem long ago;
we now have BibTeX as a file format,
and any number of programs,
like `biblatex` or `natbib`,
to generate good-looking citations from that.
Further, everyone's favourite document format converter<!--
-->—pandoc—<!--
-->has excellent support for leveraging this functionality.
All that's left is to integrate this into Hakyll;
and to play around with it, of course!

<!--more-->

# Introduction

Pandoc—using the [citeproc][gh:citeproc] library—can
make use of [CSL],
which is an XML-based formatting specification,[^5]
in order to decide how the generated citations will be displayed.
There are _a lot_ of styles to choose from;
the [Zotero Style Repository][zotero:csl] alone sports around 10'000 different ones!
I settled on one that closely resembles `biblatex`'s "alphabetic" style.

To not keep anyone in suspense, the final result looks like this:

<div class="highlight">
 <p>
  This is a line citing
  <span>[<a href="#ref-benabou67:bicats-intro" role="doc-biblioref">Béna67</a>]</span>
  and
  <span>[<a href="ref-day07:lax-intro" role="doc-biblioref">DaPaSt07</a>]</span>.
 </p>

 <h2>References</h2>
 <div id="refs-intro" class="references csl-bib-body" role="doc-bibliography">
 <table>
 <tr>
  <div id="ref-benabou67:bicats-intro">
   <td style="vertical-align: top"><div class="csl-left-margin">[Béna67] </div></td>
   <td> </td>
   <td><div class="csl-right-inline">J. Bénabou, <span>“<a href="https://doi.org/10.1007/BFb0074299">Introduction to bicategories</a>.”</span> B<span>é</span>nabou, <span>Jean</span> et al., <span>Reports</span> of the <span>Midwest</span> <span>Category</span> <span>Seminar</span>. <span>Lect</span>. <span>Notes</span> <span>Math</span>. 47, 1–77, 1967. </div></td>
  </div>
 </tr>
 <tr>
  <div id="ref-day07:lax-intro" class="csl-entry" role="doc-biblioentry">
   <td style="vertical-align: top"><div class="csl-left-margin">[DaPaSt07] </div></td>
   <td> </td>
   <td><div class="csl-right-inline">B. Day, E. Panchadcharam, and R. Street, <span>“Lax braidings and the lax centre,”</span> in <em>Hopf algebras and generalizations. AMS special session on hopf algebras at the crossroads of algebra, category theory, and topology, evanston, IL, USA, october 23–24, 2004.</em>, Providence, RI: American Mathematical Society (AMS), 2007, pp. 1–17. </div></td>
  </div>
 </tr>
 </table>
 </div>
</div>

The rest of the post will be a step by step explanation of how I arrived at this result,
hopefully in a generic enough way such that the ideas presented here
may translate to other problems that can be solved with pandoc filters.
If you just want the code, however, the relevant commits are
[here][site:impl:citation-support],
[here][site:impl:citation-css],
and [here][site:impl:citation-table].[^8]

# Simple setup

On the command line,
the incantation one needs to write is quite simple:

``` shell
$ pandoc --from=markdown --to=html                               \
         --citeproc --biblatex                                   \
         --csl=bib/style.csl --bibliography=bib/bibliography.bib \
         FILE.md
```

Integrating this into a basic Hakyll setup is not much more complicated,
as some kind soul has given us
[Hakyll.Web.Pandoc.Biblio]—<!--
-->a library specifically written to make use of pandoc's bibliography handling.

In fact, [Jasper Van der Jeugt] himself created a [tutorial][ghub:hakyll-citeproc] for this.
Slightly abbreviated, it goes a bit like the following.
First, we compile the CSL and BibTeX files in our main function:

``` haskell
main :: IO ()
main = hakyll $ do
  -- …
  match "bib/style.csl"        $ compile cslCompiler
  match "bib/bibliography.bib" $ compile biblioCompiler
  -- …
```

Next, we create a compiler
with the help of the `readPandocBiblio` function:

``` haskell
myPandocBiblioCompiler :: Compiler (Item String)
myPandocBiblioCompiler = do
  csl <- load "bib/style.csl"
  bib <- load "bib/bibliography.bib"
  getResourceBody
    >>= readPandocBiblio defaultHakyllReaderOptions csl bib
    >>= pure . writePandoc
```

Since this kind of setup is so common,
it is already packaged up in `pandoc​Biblio​Compiler`:

``` haskell
myPandocBiblioCompiler :: Compiler (Item String)
myPandocBiblioCompiler =
  pandocBiblioCompiler "bib/style.csl" "bib/bibliography.bib"
```

With all these pieces in place,
this compiler can now be used in place of the default `pandocCompiler`;
for example, instead of

``` haskell
  -- somewhere in main
  compile $ pandocCompiler
        >>= loadAndApplyTemplate "default.html" defaultContext
```

one would write

``` haskell
  -- somewhere in main
  compile $ myPandocBiblioCompiler
        >>= loadAndApplyTemplate "default.html" defaultContext
```

And that's all there is to it!
Citations should now work out of the box using the `[@citation-name]` syntax.
They appear like this:

<div id="ref-first-try" class="highlight">
 <p>
  This is a line citing <span>[Béna67]</span>, and <span>[DaPaSt07]</span>.
 </p>

 <div>
  <div>
   <div>[Béna67] </div>
   <div>J. Bénabou, <span>“<a href="https://doi.org/10.1007/BFb0074299">Introduction to bicategories</a>.”</span> B<span>é</span>nabou, <span>Jean</span> et al., <span>Reports</span> of the <span>Midwest</span> <span>Category</span> <span>Seminar</span>. <span>Lect</span>. <span>Notes</span> <span>Math</span>. 47, 1–77, 1967. </div>
  </div>
  <div>
   <div>[DaPaSt07] </div>
   <div>B. Day, E. Panchadcharam, and R. Street, <span>“Lax braidings and the lax centre,”</span> in <em>Hopf algebras and generalizations. AMS special session on hopf algebras at the crossroads of algebra, category theory, and topology, evanston, IL, USA, october 23–24, 2004.</em>, Providence, RI: American Mathematical Society (AMS), 2007, pp. 1–17. </div>
  </div>
 </div>
</div>

While the basic setup really is this easy,
incorporating citations into a real-world Hakyll code base
proves to be slightly more difficult<!--
-->—not to mention addressing all of my neuroses.
Let's get straight into it.

# Integration into my Hakyll setup

The most important bit is that,
instead of a separate compiler,
I would really rather have a [pandoc filter][pandoc:filter] for this feature.
Briefly, filters are transformations of
pandoc's internal representation of a document's structure.[^2]
This internal representation is encapsulated in the `Pandoc` type

``` haskell
data Pandoc = Pandoc Meta [Block]
```

which represents a full document plus some metadata.
The [Block][pandoc:type:block] and [Inline][pandoc:type:inline] types then
contain more fine-grained stylistic information,
like the presence of lists, tables, bold text, and so on.
Writing a filter is further simplified by the [Walkable][pandoc:type:walkable]
type class,
with which it becomes trivial to promote an `Inline -> Inline` function
to a full `Pandoc -> Pandoc` transformation.

Luckily, `Hakyll.​Web.​Pandoc.​Biblio` exposes a function for this kind of use-case:

``` haskell
processPandocBiblio
  :: Item CSL -> Item Biblio               -- Formatting boiler plate
  -> Item Pandoc -> Compiler (Item Pandoc) -- The actual transformation
```

A basic filter materialises:

``` haskell
processBib :: Item Pandoc -> Compiler (Item Pandoc)
processBib pandoc = do
  csl <- load "bib/style.csl"
  bib <- load "bib/bibliography.bib"
  processPandocBiblio csl bib pandoc
```

Notice that the type of this function transforms an `Item Pandoc`!
An [Item][hakyll:type:item] is a type internal to Hakyll,
which associates a unique identifier to some contents.
In the best case,
one would like to treat this as an implementation detail and not think about it at all.
However, most of Hakyll's other pandoc functions that let you do AST
transformations—like `pandoc​Compiler​With​TransformM`—have a bit of a different API,
accepting only a `Pandoc -> Compiler Pandoc` argument.
This means that some care is needed to get everything to type check.

To start, my personal pandoc compiler looks a little bit like this:

``` haskell
myPandocCompiler :: Compiler (Item String)
myPandocCompiler =
  pandocCompilerWithTransformM
    myReader
    myWriter
    myTransformations   -- Pandoc -> Compiler Pandoc
```

The easiest way I found to incorporate a `process​Pandoc​Biblio`-like transformation into this was to
write a function that's like `pandoc​Compiler​With​TransformM`,
but accepts a wider input range.
Looking at its definition
already gives some idea as to what needs to be done:

``` haskell
pandocCompilerWithTransformM
  :: ReaderOptions -> WriterOptions
  -> (Pandoc -> Compiler Pandoc)
  -> Compiler (Item String)
pandocCompilerWithTransformM ropt wopt f =
  getResourceBody >>= renderPandocWithTransformM ropt wopt f

renderPandocWithTransformM
  :: ReaderOptions -> WriterOptions
  -> (Pandoc -> Compiler Pandoc)
  -> Item String
  -> Compiler (Item String)
renderPandocWithTransformM ropt wopt f i =
  writePandocWith wopt <$> (traverse f =<< readPandocWith ropt i)

-- readPandocWith :: ReaderOptions -> Item String -> Compiler (Item Pandoc)
```

So `pandoc​Compiler​With​TransformM` is defined in terms of `render​Pandoc​With​TransformM`,
which in turn has quite a simple implementation.
Notice in particular the `traverse f =<< readPandocWith ropt i` bit;
`readPandocWith` returns a `Compiler (Item Pandoc)`,
so the `traverse` above exactly transform our `f` into a function that works at the `Item` level.
Omitting this yields the desired functions:

``` haskell
myRenderPandocWithTransformM
  :: ReaderOptions -> WriterOptions
  -> (Item Pandoc -> Compiler (Item Pandoc))  -- this changed!
  -> Item String
  -> Compiler (Item String)
myRenderPandocWithTransformM ropt wopt f i =
  writePandocWith wopt <$> (f =<< readPandocWith ropt i)

myPandocCompilerWithTransformM
  :: ReaderOptions -> WriterOptions
  -> (Item Pandoc -> Compiler (Item Pandoc))  -- this changed!
  -> Compiler (Item String)
myPandocCompilerWithTransformM ropt wopt f =
  getResourceBody >>= myRenderPandocWithTransformM ropt wopt f
```

We can now easily incorporate the `processBib` function defined above into our existing framework,
adding a `traverse` where the old code was:

``` haskell
myPandocCompiler :: Compiler (Item String)
myPandocCompiler =
  myPandocCompilerWithTransformM
    myReader
    myWriter
    (    traverse myTransformations  -- Item Pandoc -> Compiler (Item Pandoc)
     <=< processBib                  -- composed with the new stuff
    )
```

And that's it!
Citations can now be added using the `[@citation-name]` syntax mentioned above,
and they look exactly the same as in <a href="#ref-first-try">the simple example</a>.

Now, that particular formatting looks *fine*,
but I'm sure you can already discern a few not-so-nice bits.
Let's address the most glaring ones,
at least in my opinion.

# Changing the look

## Adding a header for the references

As it stands now,
the references are just dumped at the bottom of the page,
without any additional heading.
This looks subjectively ugly,
so automatically adding one
whenever at least one citation is present would be nice.

There is another small complication because of my idiosyncratic Hakyll setup:
in order to easily control the style of the headings in the table of contents,
I pre-generate the TOC before the actual compilation of the site.
This means that in addition to the `processBib` function,
we need to change the code in one other place.

The generation looks a little bit like this:

``` haskell
getTocCtx :: Context a -> Compiler (Context a)
getTocCtx ctx = do
  writerOpts <- mkTocWriter
  toc        <- renderPandocWith
                  defaultHakyllReaderOptions
                  writerOpts
                  =<< getResourceBody
  pure $ mconcat [ ctx
                 , constField "toc" $ doStuffWithTheToc toc
                 ]
```

When the TOC is saved in the `toc` variable,
it's already rendered into a string,
which means that it's time for some good old string manipulations.
The (simplified) HTML for a typical table of contents looks like this:

``` html
<div>
  <p>Contents</p>
  <ul>
    <li>
      High level structure
      <ul>
        <li>Topics</li>
        <li>Files</li>
      </ul>
    </li>
  </ul>
</div>
```

The last `</ul>` block seems to be an appropriate target to attack.
The function to add a `References` heading in its place is swiftly written:

``` haskell
addBibHeading :: String -> String
addBibHeading s = T.unpack . mconcat $
  [ T.dropEnd 5 before
  , "<li><a href=\"#references\">References</a></li></ul>"
  , after
  ]
 where
  (before, after) = T.breakOnEnd "</ul>" (T.pack s)
```

This can now be incorporated into `getTocCtx` in a straightforward manner:[^1]

``` haskell
getTocCtx :: Context a -> Compiler (Context a)
getTocCtx ctx = do
  writerOpts <- mkTocWriter
  toc        <- renderPandocWith
                  defaultHakyllReaderOptions
                  writerOpts
                  =<< getResourceBody
  pure $ mconcat [ ctx
                 , constField "toc" $ addBibHeading (doStuffWithTheToc toc)
                                   -- ^^^^^^^^^^^^^
                 ]
```

All we need to do now is to actually create that header.
Again inspecting the HTML, one can spy a line along the lines of

``` html
<div id="refs" class="references csl-bib-body" role="doc-bibliography">
```

when the references start.
Inserting a heading above that sounds like a plan.
This can again be done using filters,
this time inside our `processBib` function:[^6]

``` haskell
processBib :: Item Pandoc -> Compiler (Item Pandoc)
processBib pandoc = do
  csl <- load "bib/style.csl"
  bib <- load "bib/bibliography.bib"
  fmap insertRefHeading <$> processPandocBiblio csl bib pandoc
  --   ^^^^^^^^^^^^^^^^
 where
  -- Insert a heading for the citations.
  insertRefHeading :: Pandoc -> Pandoc
  insertRefHeading = walk $ concatMap \case
    d@(Div ("refs", _, _) _) ->
      [Header 1 ("references", [], []) [Str "References"], d]
    block -> [block]
```

The citations now look like this:

<div class="highlight">
 <p>
  This is a line citing <span>[Béna67]</span>, and <span>[DaPaSt07]</span>.
 </p>

 <h2>References</h2>
 <div>
  <div>
   <div>[Béna67] </div>
   <div>J. Bénabou, <span>“<a href="https://doi.org/10.1007/BFb0074299">Introduction to bicategories</a>.”</span> B<span>é</span>nabou, <span>Jean</span> et al., <span>Reports</span> of the <span>Midwest</span> <span>Category</span> <span>Seminar</span>. <span>Lect</span>. <span>Notes</span> <span>Math</span>. 47, 1–77, 1967. </div>
  </div>
  <div>
   <div>[DaPaSt07] </div>
   <div>B. Day, E. Panchadcharam, and R. Street, <span>“Lax braidings and the lax centre,”</span> in <em>Hopf algebras and generalizations. AMS special session on hopf algebras at the crossroads of algebra, category theory, and topology, evanston, IL, USA, october 23–24, 2004.</em>, Providence, RI: American Mathematical Society (AMS), 2007, pp. 1–17. </div>
  </div>
 </div>
</div>

## Prettifying the generated references

What immediately irks me in the above output is
that a single citation is broken up into two lines.
Thankfully, this is easily fixed by a tiny bit of CSS.

``` css
/* Don't split up a citation over multiple lines. */
div.csl-left-margin {
    display: inline;
}
div.csl-right-inline {
    display: inline;
}
```

Much better:

<div class="highlight">
 <p>
  This is a line citing <span>[Béna67]</span> and <span>[DaPaSt07]</span>.
 </p>

 <h2>References</h2>
 <div>
  <div>
   <div class="csl-left-margin">[Béna67] </div>
   <div class="csl-right-inline">J. Bénabou, <span>“<a href="https://doi.org/10.1007/BFb0074299">Introduction to bicategories</a>.”</span> B<span>é</span>nabou, <span>Jean</span> et al., <span>Reports</span> of the <span>Midwest</span> <span>Category</span> <span>Seminar</span>. <span>Lect</span>. <span>Notes</span> <span>Math</span>. 47, 1–77, 1967. </div>
  </div>
  <div>
   <div class="csl-left-margin">[DaPaSt07] </div>
   <div class="csl-right-inline">B. Day, E. Panchadcharam, and R. Street, <span>“Lax braidings and the lax centre,”</span> in <em>Hopf algebras and generalizations. AMS special session on hopf algebras at the crossroads of algebra, category theory, and topology, evanston, IL, USA, october 23–24, 2004.</em>, Providence, RI: American Mathematical Society (AMS), 2007, pp. 1–17. </div>
  </div>
 </div>
</div>

Next, notice that there is no link from the label in the text
to the actual citation at the end.
This seems pretty inconvenient,
as at least I often jump to citations whose label is
unfamiliar—just to get an idea what kind of article it is.

Pandoc does accept a `link-citations` option that controls this behaviour,
which works fine for my purposes.
Setting this can be done directly
by modifying the [Meta][pandoc:meta] field of the `Pandoc` type:

``` haskell
processBib :: Item Pandoc -> Compiler (Item Pandoc)
processBib pandoc = do
  csl <- load "bib/style.csl"
  bib <- load "bib/bibliography.bib"
  -- We do want to link citations.
  p <- withItemBody
         (\(Pandoc (Meta meta) bs) -> pure $
           Pandoc (Meta $ Map.insert "link-citations" (MetaBool True) meta)
                  bs)
         pandoc
  fmap insertRefHeading <$> processPandocBiblio csl bib pandoc
```

One could also introduce a Hakyll metadata field
if this is to be done conditionally,
but I see no reason to not link citations,
so I didn't.

Everything works as expected:

<div class="highlight">
 <p>
  This is a line citing
  <span>[<a href="#ref-benabou67:bicats-1" role="doc-biblioref">Béna67</a>]</span>
  and
  <span>[<a href="ref-day07:lax-1" role="doc-biblioref">DaPaSt07</a>]</span>.
 </p>

 <h2>References</h2>
 <div id="refs1" class="references csl-bib-body" role="doc-bibliography">
  <div id="ref-benabou67:bicats-a">
   <div class="csl-left-margin">[Béna67] </div>
   <div class="csl-right-inline">J. Bénabou, <span>“<a href="https://doi.org/10.1007/BFb0074299">Introduction to bicategories</a>.”</span> B<span>é</span>nabou, <span>Jean</span> et al., <span>Reports</span> of the <span>Midwest</span> <span>Category</span> <span>Seminar</span>. <span>Lect</span>. <span>Notes</span> <span>Math</span>. 47, 1–77, 1967. </div>
  </div>
  <div id="ref-day07:lax-1" class="csl-entry" role="doc-biblioentry">
   <div class="csl-left-margin">[DaPaSt07] </div>
   <div class="csl-right-inline">B. Day, E. Panchadcharam, and R. Street, <span>“Lax braidings and the lax centre,”</span> in <em>Hopf algebras and generalizations. AMS special session on hopf algebras at the crossroads of algebra, category theory, and topology, evanston, IL, USA, october 23–24, 2004.</em>, Providence, RI: American Mathematical Society (AMS), 2007, pp. 1–17. </div>
  </div>
 </div>
</div>

Lastly,
and this is perhaps the most important modification,
I think that not having a table-like
look—aligning labels and not letting the citation information run under its label—looks
a bit awkward.
As so many times before, pandoc filters come to the rescue here.
There is a `Table` constructor of the `Block` type which we will use:

``` haskell
Table :: Attr -> Caption -> [ColSpec] -> TableHead -> [TableBody] -> TableFoot
      -> Block
```

This may look kind of scary,
but for our simple use-case there is the aptly named `simpleTable` function:

``` haskell
simpleTable :: [Blocks]   -- ^ Headers
            -> [[Blocks]] -- ^ Rows
            -> Blocks

-- where
type Blocks    = Many Block
newtype Many a = Many (Seq a)
```

Basically, any particular cell is composed of a number of `Blocks`,
a single row is a bunch (list) of those,
and all rows taken together then form a `[[Blocks]]`.
Equipped with this knowledge,
we can just search for an instance of a citation,
which will be two `Div`s inside of one `Para` inside of one `Div`,
and replace accordingly:

``` haskell
-- | Align all citations in a table.
tableiseBib :: Pandoc -> Pandoc
tableiseBib = walk \case
  -- Citations start with a <div id="refs" …>
  Div a@("refs", _, _) body ->
    -- No header needed, we just want to fill in the body contents.
    Div a (Many.toList (simpleTable [] (map citToRow body)))
  body -> body
 where
  citToRow :: Block -> [Many Block]
  citToRow = map Many.singleton . \case
    Div attr [Para [s1, s2]] ->
      [Div attr [Plain [s1]], Plain [Space], Plain [s2]]
    _ -> error "citToRow: unexpected citation format."
```

Just signaling an `error` here
in case of an unexpected format
was nice for debugging the code<!--
-->—I missed the `Para` at first—<!--
-->and at this point I see no reason to change it.
Perhaps it is better to fail fast in theses kinds of situations,
instead of trying to desperately produce something based off garbage input.

The `tableiseBib` function can
be incorporated into `processBib`
in a straightforward fashion:

``` haskell
processBib :: Item Pandoc -> Compiler (Item Pandoc)
processBib pandoc = do
  -- …
  fmap (tableiseBib . insertRefHeading) <$> processPandocBiblio csl bib p
```

All that's left is to nicely align everything:
the right side of the table will,
in general,
span multiple lines—in stark contrast to the label.
This is again just a few lines of CSS:

``` css
/* Align citations to the top. */
div#refs td {
  vertical-align: top;
}
```

We get the expected result already showcased in the introduction.
For completeness, here it is again:

<div class="highlight">
 <p>
  This is a line citing
  <span>[<a href="#ref-benabou67:bicats" role="doc-biblioref">Béna67</a>]</span>
  and
  <span>[<a href="ref-day07:lax" role="doc-biblioref">DaPaSt07</a>]</span>.
 </p>

 <h2>References</h2>
 <div id="refs-final" class="references csl-bib-body" role="doc-bibliography">
 <table>
 <tr>
  <div id="ref-benabou67:bicats">
   <td style="vertical-align: top"><div class="csl-left-margin">[Béna67] </div></td>
   <td> </td>
   <td><div class="csl-right-inline">J. Bénabou, <span>“<a href="https://doi.org/10.1007/BFb0074299">Introduction to bicategories</a>.”</span> B<span>é</span>nabou, <span>Jean</span> et al., <span>Reports</span> of the <span>Midwest</span> <span>Category</span> <span>Seminar</span>. <span>Lect</span>. <span>Notes</span> <span>Math</span>. 47, 1–77, 1967. </div></td>
  </div>
 </tr>
 <tr>
  <div id="ref-day07:lax" class="csl-entry" role="doc-biblioentry">
   <td style="vertical-align: top"><div class="csl-left-margin">[DaPaSt07] </div></td>
   <td> </td>
   <td><div class="csl-right-inline">B. Day, E. Panchadcharam, and R. Street, <span>“Lax braidings and the lax centre,”</span> in <em>Hopf algebras and generalizations. AMS special session on hopf algebras at the crossroads of algebra, category theory, and topology, evanston, IL, USA, october 23–24, 2004.</em>, Providence, RI: American Mathematical Society (AMS), 2007, pp. 1–17. </div></td>
  </div>
 </tr>
 </table>
 </div>
</div>

Neat.

# Conclusion

Since smart people had already written all the hard parts,
this was surprisingly easy to add for such a useful feature!
Plus, playing around with pandoc filters is always fun.

Especially the `my​{render​Pandoc,Pandoc​Compiler}​With​TransformM` functions
could—with different names, of course—perhaps be contributed to upstream Hakyll.
A variant of any of the `*PandocBiblio` functions
that explicitly accepts a list of additional arguments
to give to `citeproc` might also be useful;
there are quite a few [metadata fields][pandoc:citations] one can specify,
after all.
Finally, I think a format along the lines of `tableiseBib` would be quite nice to have with label-style citations.
However, the current implementation is much too specific to justify living anywhere but a personal configuration.
Some day, maybe.

[CSL]: https://citationstyles.org/
[Hakyll.Web.Pandoc.Biblio]: https://hackage.haskell.org/package/hakyll-4.16.0.0/docs/Hakyll-Web-Pandoc-Biblio.html#v:pandocBiblioCompiler
[Jasper Van der Jeugt]: https://jaspervdj.be/
[gh:citeproc]: https://github.com/jgm/citeproc
[ghub:hakyll-citeproc]: https://github.com/jaspervdj/hakyll-citeproc-example
[hakyll:type:item]: https://hackage.haskell.org/package/hakyll/docs/Hakyll-Core-Item.html#t:Item
[pandoc:citations]: https://pandoc.org/MANUAL.html#citations
[pandoc:filter]: https://pandoc.org/filters.html
[pandoc:meta]: https://hackage.haskell.org/package/pandoc-types-1.23/docs/Text-Pandoc-Definition.html#t:Meta
[pandoc:type:block]: https://hackage.haskell.org/package/pandoc-types/docs/Text-Pandoc-Definition.html#t:Block
[pandoc:type:inline]: https://hackage.haskell.org/package/pandoc-types/docs/Text-Pandoc-Definition.html#t:Inline
[pandoc:type:walkable]: https://hackage.haskell.org/package/pandoc-types/docs/Text-Pandoc-Walk.html#t:Walkable
[post:pygments]: ./pygmentising-hakyll.html
[post:sidenotes]: ./block-sidenotes.html
[site:impl:citation-css]: https://github.com/slotThe/slotThe.github.io/commit/c23b03522fcddca779f2a1e593a32b2e51284958
[site:impl:citation-support]: https://github.com/slotThe/slotThe.github.io/commit/62c6072243ee06d8df39813b7e35a6fd0ea1fe9d
[site:impl:citation-table]: https://github.com/slotThe/slotThe.github.io/commit/f39352fd825ed6efad918de11b002734fdff03c4
[zotero:csl]: https://www.zotero.org/styles

[^1]:
  {-} 󠀠

      󠀠

      In the actual code,
      I also check for a `bib` boolean field,
      in order to decide whether this transformation should actually be applied.
      If you are interested in that, see the relevant [commit][site:impl:citation-support].

[^2]: As one can imagine, the use-cases for filters are manifold.
      From changing Hakyll's default [syntax highlighting][post:pygments],
      to swapping out footnotes and producing this very [sidenote][post:sidenotes],
      almost anything one can imagine is possible.

[^5]: If you want some more information,
      the format describes itself as

      > an XML-based format to describe the formatting of citations,
      > notes and bibliographies, offering:
      >
      > - An open format
      > - Compact and robust styles
      > - Extensive support for style requirements
      > - Automatic style localization
      > - Infrastructure for style distribution and updating
      > - Thousands of freely available styles (Creative Commons BY-SA licensed)

[^6]:
  {-} 󠀠

      󠀠

      󠀠

      󠀠

      󠀠

      󠀠

      This example also nicely showcases the power of the `Walkable` type class.
      Via the

      ``` haskell
      Walkable Block Pandoc
      ```

      instance,
      I seamlessly walk over all `Block`s in the AST
      and pick out the ones I'd like to change.
      Pretty neat if you ask me.

[^8]: While this style is good enough for now,
      it's still not quite perfect;
      suggestions for other styles would be most welcome!
