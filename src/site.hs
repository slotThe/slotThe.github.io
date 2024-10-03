{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

import Data.Text    qualified as T
import Data.Text.IO qualified as T

import Control.Monad
import Data.List (foldl')
import Data.Maybe
import Data.String (IsString)
import Data.Text (Text)
import GHC.IO.Handle (BufferMode (NoBuffering), Handle, hSetBuffering)
import Hakyll
import Skylighting (syntaxesByFilename, defaultSyntaxMap, Syntax (sName)) -- N.b: only for marking
import System.Process (runInteractiveCommand)
import Text.HTML.TagSoup (Tag (TagClose, TagOpen), (~==))
import Text.Pandoc.Builder (simpleTable, Many, HasMeta (setMeta))
import qualified Text.Pandoc.Builder as Many (toList, singleton)
import Text.Pandoc.Definition (Block (..), Inline (..), MathType (..), Pandoc)
import Text.Pandoc.Options (Extension (..), HTMLMathMethod (..), ReaderOptions (readerExtensions), WriterOptions (..), extensionsFromList)
import Text.Pandoc.Shared (headerShift)
import Text.Pandoc.SideNoteHTML (usingSideNotesHTML)
import Text.Pandoc.Templates (compileTemplate)
import Text.Pandoc.Walk (walk, walkM)


siteURL :: IsString s => s
siteURL = "https://tony-zorman.com"

main :: IO ()
main = hakyllWith defaultConfiguration{ destinationDirectory = "docs" } do
  -- Housekeeping
  match "templates/*" $ compile templateBodyCompiler
  match "css/*" do
    route   idRoute
    compile compressCssCompiler
  match (     "favicon.png"
         .||. "images/**"
         .||. "talks/**.pdf"
         .||. "css/fonts/**"
         .||. "robots.txt"
        )
    do route   idRoute
       compile copyFileCompiler

  -- Redirects
  version "redirects" $ createRedirects redirects

  -- Citations
  match "bib/style.csl"        $ compile cslCompiler    -- labels: [DaPaSt07]
  match "bib/bibliography.bib" $ compile biblioCompiler

  -- Generate tags
  tags <- buildTags "posts/**" (fromCapture "tags/**.html")
  let tagCtx = tagsField "tags" tags <> postCtx

  -- Build some pages!
  landingPage tagCtx
  posts       tagCtx
  listOfPosts tags
  aboutMe
  standalones tagCtx
  rss         tags

landingPage :: Context String -> Rules ()
landingPage tagCtx = match "index.html" do
  route idRoute
  compile do
    allposts <- recentFirst =<< loadAllSnapshots allPosts "post-teaser"
    let teaserCtx = teaserField "teaser" "post-teaser" <> tagCtx
        indexCtx  = listField "posts" teaserCtx (pure allposts) <> bodyField "body"
    getResourceBody
      >>= applyAsTemplate                               indexCtx
      >>= loadAndApplyTemplate "templates/default.html" indexCtx
      >>= withItemBody (pure . recountSidenotes)
      >>= relativizeUrls
 where
  -- Recount sidenotes for the landing page, as otherwise they all have the
  -- same IDs, resulting in invalid HTML and weirdly behaving sidenotes when
  -- they are clickable. The format is something like
  --
  --     <label for="sn-0" …></label><… id="sn-0" …/>
  --
  -- so we need to count in steps of two.
  recountSidenotes :: String -> String
  recountSidenotes = asTxt (go 0)
   where
    go :: Double -> Text -> Text
    go n str = case T.breakOn "=\"sn-" str of
      (r, "") -> r
      (h, t ) -> h <> "=\"sn-" <> tshow @Int (floor n) <> "\"" <> go (n + 0.5) (killSn t)

    killSn :: Text -> Text
    killSn = let d = T.drop 1 . T.dropWhile (/= '\"') in d . d

posts :: Context String -> Rules ()
posts tagCtx = match allPosts do
  route $ setExtension "html"
  compile do
    tocCtx <- getTocCtx tagCtx
    -- Atom feeds get their own compiler; the website uses a lot of stuff
    -- (sidenotes, small-caps, …) that doesn't work in feeds.
    void $ pandocRssCompiler
       >>= loadAndApplyTemplate
             "templates/post.html"
             (tocCtx <> boolField "no-comment" (pure True))
       >>= mkCleanSnapshot "post-for-feed"  -- See 'rss'
    -- Actual compiler for a page.
    myPandocCompiler
      >>= mkCleanSnapshot "post-teaser"   -- For the previews on the main page.
      >>= loadAndApplyTemplate "templates/post.html"     tocCtx
      >>= loadAndApplyTemplate "templates/default.html"  tocCtx
      >>= relativizeUrls

-- | For showing all posts, we want a list of all posts, followed by a list of
-- tags with associated posts.
-- https://stackoverflow.com/questions/52805193/in-hakyll-how-can-i-generate-a-tags-page
listOfPosts :: Tags -> Rules ()
listOfPosts tags@Tags{ tagsMakeId, tagsMap } = do
  -- All posts
  create ["posts.html"] do
    let allPostsCtx :: Context String
          = listField "posts" postCtx (recentFirst =<< loadAll allPosts)
    mkList allPostsCtx "All posts" "atom" "templates/all-posts.html"
  -- Only posts tagged by a certain tag
  tagsRules tags \tag taggedPosts -> do
    let taggedPostCtx :: Context String
          = listField "posts" postCtx (recentFirst =<< loadAll taggedPosts)
    mkList taggedPostCtx
           ("Posts tagged " <> "\"" <> tag <> "\"")
           ("../atom-" <> tag)
           "templates/post-list.html"
 where
  mkList :: Context String -> String -> String -> Identifier -> Rules ()
  mkList = mkPostList (mkTagAssocs tagsMakeId tagsMap)

aboutMe :: Rules ()
aboutMe = do
  match (fromList ["research.md", "free-software.md"]) do
    route $ setExtension "html"
    compile do
      tocCtx <- getTocCtx defaultContext
      myPandocCompiler
        >>= loadAndApplyTemplate "templates/toc.html"     tocCtx
        >>= loadAndApplyTemplate "templates/default.html" tocCtx
        >>= relativizeUrls

  match (fromList ["about.md", "impressum.md"]) do
    route   $ setExtension "html"
    compile $ myPandocCompiler
          >>= loadAndApplyTemplate
                "templates/default.html"
                (  boolField "noindex" (pure True)  -- pls no index
                <> defaultContext )
          >>= relativizeUrls

standalones :: Context String -> Rules ()
standalones tagCtx = do
  -- Mackey functors seminar
  mkStandalone "mackey-functors.md" pure tagCtx Nothing
  -- Posters
  let posterGlob :: String -> Pattern
        = \pth -> fromGlob ("posters/" <> pth <> "/**") .&&. complement "**.md"
  --- CT2024
  match (posterGlob "ct2024") do
    route   idRoute
    compile copyFileCompiler
  mkStandalone "posters/ct2024/ct2024.md" pure tagCtx (Just "ct2024.html")
  --- Ferrara 2024
  match (posterGlob "ferrara2024") do
    route   idRoute
    compile copyFileCompiler
  mkStandalone "posters/ferrara2024/ferrara2024.md" pure tagCtx (Just "ferrara2024.html")
  -- Git introduction
  let gitCtx title = constField "title" title <> tagCtx
      fixTranscript = withItemBody $ pure .                  -- I know…
        asTxt (T.replace "./transcript.md" "./git-introduction/transcript.html")
  mkStandalone "talks/git-introduction.md"
               fixTranscript
               (gitCtx "Git Introduction" <> constField "no-toc" "true")
               Nothing
  mkStandalone "talks/git-introduction/transcript.md"
               pure
               (gitCtx "How to Use Git—an Interactive Tutorial")
               Nothing
  -- Key
  match "key.txt" do
    route idRoute
    compile copyFileCompiler
  -- 404 page
  match "404.html" do
    route idRoute
    compile $ getResourceBody >>= relativizeUrls
 where
  -- Compile a standalone site.
  mkStandalone
    :: Pattern                                 -- Match the main standalone page
    -> (Item String -> Compiler (Item String))  -- Additional compilation step
    -> Context String                          -- Context
    -> Maybe String                            -- Alternative route
    -> Rules ()
  mkStandalone ptn action baseCtx altRoute = match ptn do
    route $ maybe (setExtension "html") constRoute altRoute
    compile do
      ctx <- getTocCtx baseCtx
      myPandocCompiler
        >>= loadAndApplyTemplate "templates/post.html"       ctx
        >>= loadAndApplyTemplate "templates/standalone.html" ctx
        >>= action
        >>= relativizeUrls

rss :: Tags -> Rules ()
rss tags = do
  -- All posts
  create ["atom.xml"] do
    route idRoute
    compile do
      lastPosts <- recentFirst =<< loadAllSnapshots allPosts "post-for-feed"
      renderAtom feedConfig (postCtx <> bodyField "description") lastPosts
  -- Individual tags
  tagsRules tags $ \tag taggedPosts ->
    create [fromFilePath $ "atom-" <> tag <> ".xml"] do
      route idRoute
      compile do
        lastPosts <- recentFirst =<< loadAllSnapshots taggedPosts "post-for-feed"
        renderAtom feedConfig (postCtx <> bodyField "description") lastPosts
 where
  feedConfig :: FeedConfiguration
  feedConfig = FeedConfiguration
    { feedTitle       = "Tony Zorman · Blog"
    , feedDescription = "Maths, Haskell, Emacs, and whatever else comes to mind."
    , feedAuthorName  = "Tony Zorman"
    , feedAuthorEmail = "tonyzorman@mailbox.org"
    , feedRoot        = siteURL
    }

-----------------------------------------------------------------------
-- Contexts

postCtx :: Context String
postCtx = dateField "date" "%F" <> estimatedReadingTime <> defaultContext
 where
  estimatedReadingTime :: Context String
  estimatedReadingTime = field "estimated-reading-time" $ \key ->
    let ws   :: Int = length . words . stripTags . itemBody $ key
        mins :: Int = ceiling @Double (fromIntegral ws / 250)
     in pure $ addTitle (show ws <> " words") (show mins <> " min read")

-- | Augment the 'defaultContext' with a list of all tags, as well as
-- all posts associated to a given tag.
defaultCtxWithTags :: Compiler [Item [Identifier]] -> Context String
defaultCtxWithTags tagAssocs = listField "tags" tagCtx tagAssocs <> defaultContext
 where
  tagCtx :: Context [Identifier]
  tagCtx -- we are inside a tag context now.
     = listFieldWith "posts" postCtx (recentFirst <=< traverse load . itemBody)
    <> metadataField
    <> titleField "title"

-- | Create a context that contains a table of contents.
-- The main sources here are
--
--   + TOC         : https://svejcar.dev/posts/2019/11/27/table-of-contents-in-hakyll/
--   + TOC toggling: https://github.com/duplode/duplode.github.io/blob/sources/src/site.hs
--
-- However, note that this is /not/ part of 'myPandocCompiler' for good
-- reasons. I also want section links as part of the heading (see
-- 'myPandocCompiler'), but these should not show up in the table of
-- contents for obvious reasons.
--
-- Hence, this function
--
--     (i) creates a writer that has my preferred writer settings,
--
--    (ii) already renders this with pandoc, and
--
--   (iii) adds the finished thing as a constant field to the metadata
--         of the current entry.
--
-- Additionally, the @no-toc@ option is honoured insofar as a @no-toc@
-- boolean field is introduced, which can be used from the post template
-- (@.\/templates\/post.html@).
--
-- Further, because the table of contents is pre-generated here and then
-- never touched again, this function also needs to check for the @bib@
-- metadata field. If this is present, an extra "References" section is
-- added to the TOC. The actual generating of bibtex data is done in the
-- 'myPandocCompiler' function, however.
--
-- There is some CSS that makes section links only show on hover.
getTocCtx :: Context a -> Compiler (Context a)
getTocCtx ctx = do
  noToc      <- (Just "true" ==) <$> (getUnderlying >>= (`getMetadataField` "no-toc"))
  bib        <- (Just "true" ==) <$> (getUnderlying >>= (`getMetadataField` "bib"))
  writerOpts <- mkTocWriter defaultHakyllWriterOptions
  toc        <- renderPandocWith myReader writerOpts =<< getResourceBody
  pure $ mconcat [ ctx
                 , constField "toc" $
                     (if bib then addBibHeading else id) $ killLinkIds (itemBody toc)
                 , if noToc then boolField "no-toc" (pure noToc) else mempty
                 ]
 where
  mkTocWriter :: WriterOptions -> Compiler WriterOptions
  mkTocWriter writerOpts = do
    tmpl <- either (const Nothing) Just <$> unsafeCompiler (compileTemplate "" "$toc$")
    -- Headings will NOT be shifted down by this point because this
    -- happens before `myPandocCompiler'.
    pure $ writerOpts
      { writerTableOfContents = True
      , writerTOCDepth        = 3
      , writerTemplate        = tmpl
      }

  -- Pandoc adds IDs for its table of contents.[1, 2] However, since the
  -- site has two TOCs (where only one is shown at a given time via
  -- CSS), this results in two elements having the same identifier,
  -- which is invalid HTML. This may or may not matter, but it's
  -- certainly better to fix it.
  --
  -- [1]: https://github.com/jgm/pandoc/issues/7907
  -- [2]: https://github.com/jgm/pandoc/pull/7913
  killLinkIds :: String -> String
  killLinkIds = asTxt (mconcat . go . T.splitOn "id=\"toc-")
   where
    go :: [Text] -> [Text]
    go = \case
      []     -> []
      x : xs -> x : map (T.drop 1 . T.dropWhile (/= '\"')) xs

  -- If needed, add a heading for the bibliography at the very end of
  -- the TOC.
  addBibHeading :: String -> String
  addBibHeading = asTxt \s ->
    let (before, after) = T.breakOnEnd "</ul>" s
     in mconcat [ T.dropEnd 5 before
                , "<li><a href=\"#references\">References</a></li></ul>"
                , after
                ]

-----------------------------------------------------------------------
-- Util

allPosts :: Pattern
allPosts = "posts/**.md"

addTitle :: String -> String -> String
addTitle title body = "<span title=\"" <> title <> "\">" <> body <> "</span>"

asTxt :: (Text -> Text) -> String -> String
asTxt f = T.unpack . f . T.pack

tshow :: Show a => a -> Text
tshow = T.pack . show

--- Tags

-- | Associate every @s@ in @tagsMap@ to the list @[id]@.  In reality,
-- @s@ is a tag and @[id]@ is a list of identifiers of posts that are
-- tagged with that tag.
mkTagAssocs :: (s -> Identifier) -> [(s, [id])] -> Compiler [Item [id]]
mkTagAssocs tagsMakeId = pure . map (\(s, tgs) -> Item (tagsMakeId s) tgs)

-- | Create a post list and link to the relevant feed.
mkPostList
  :: Compiler [Item [Identifier]]
  -> Context String
  -> String         -- ^ Title of the page
  -> String         -- ^ How the feed should be called
  -> Identifier
  -> Rules ()
mkPostList tags ctx title feedName template = do
  let ctx' = ctx
          <> constField "title" title <> constField "titleWithFeed" addFeedInfo
          <> defaultCtxWithTags tags
  route idRoute
  compile $ makeItem ""
        >>= loadAndApplyTemplate template                 ctx'
        >>= loadAndApplyTemplate "templates/default.html" ctx'
        >>= relativizeUrls
 where
  -- Add feed information to the right side of a line.
  addFeedInfo :: String
  addFeedInfo = mconcat
    [ title <> " "
    , "<div class=\"floatright\">"
    ,   "<a href=\"" <> feedName <> ".xml\">feed</a>"
    , "</div>"
    ]

--- Snapshots

-- | Create a snapshot of the current layout of the site, but suppress
-- some information.  This is used to create "clean" images of, for
-- example, an article for the RSS feed.
mkCleanSnapshot ::  String -> Item String -> Compiler (Item String)
mkCleanSnapshot name item = item <$
  saveSnapshot name (withTagList (noPilcrow . supressToc) <$> item)
 where
  noPilcrow  = killTags (~== TagOpen ("a" :: String) [("class", "floatright sec-link")]) (== TagClose "a")
  supressToc = killTags (==  TagOpen "div"           [("id"   , "contents")])            (== TagClose "div")

  -- Find @open@ and kill everything between it and @close@.
  killTags :: (Tag String -> Bool) -> (Tag String -> Bool) -> [Tag String] -> [Tag String]
  killTags open close = go
   where
    go :: [Tag String] -> [Tag String]
    go tgs = case post of
      [] -> pre
      ps -> pre <> go ps
     where
      (pre, (_, post)) = fmap (fmap (drop 1) . break close) . break open $ tgs

-----------------------------------------------------------------------
-- Compilers

-- | Like 'renderPandocWithTransformM', but work on an 'Item' 'Pandoc'
-- instead of just a 'Pandoc'.
myRenderPandocWithTransformM
  :: ReaderOptions -> WriterOptions
  -> (Item Pandoc -> Compiler (Item Pandoc))
  -> Item String
  -> Compiler (Item String)
myRenderPandocWithTransformM ropt wopt f i =
    writePandocWith wopt <$> (f =<< readPandocWith ropt i)

-- | Like 'pandocCompilerWithTransformM', but work on an 'Item' 'Pandoc'
-- instead of just a 'Pandoc'.
myPandocCompilerWithTransformM
  :: ReaderOptions -> WriterOptions
  -> (Item Pandoc -> Compiler (Item Pandoc))
  -> Compiler (Item String)
myPandocCompilerWithTransformM ropt wopt f =
    getResourceBody >>= myRenderPandocWithTransformM ropt wopt f

myWriter :: WriterOptions
myWriter = defaultHakyllWriterOptions{ writerHTMLMathMethod = KaTeX "" }

myReader :: ReaderOptions
myReader = defaultHakyllReaderOptions
  { readerExtensions =
      readerExtensions defaultHakyllReaderOptions
        <> extensionsFromList [Ext_tex_math_single_backslash]
  }

-- | A simple pandoc compiler for RSS/Atom feeds, with none of the
-- fanciness that 'myPandocCompiler' has.
pandocRssCompiler :: Compiler (Item String)
pandocRssCompiler = pandocCompilerWorker pure

pandocCompilerWorker :: (Item Pandoc -> Compiler (Item Pandoc)) -> Compiler (Item String)
pandocCompilerWorker =
  myPandocCompilerWithTransformM
    myReader
    myWriter
    -- Only the `title' should be <h1>. This needs to be here so that
    -- headings are shifted down for both 'myPandocCompiler', and
    -- 'pandocRssCompiler' .
    . ((fmap . fmap) (headerShift 1) .)

-- | Pandoc compiler with syntax highlighting (via @pygmentize@),
-- sidenotes instead of footnotes (see @css/sidenotes.css@ and
-- @src/Sidenote.hs@), automatic small-caps for certain abbreviations,
-- section links, and referencing via bibtex. Also see 'getTocCtx' for
-- the generation of a table of contents (see there for an explanation),
-- and @./build.sh@ for LaTeX rendering.
myPandocCompiler :: Compiler (Item String)
myPandocCompiler =
  pandocCompilerWorker $
    traverse
      (   pure . usingSideNotesHTML myWriter
      <=< pygmentsHighlight
      <=< hlKaTeX
      -- ↑ render HMTL in various forms and ↓ do not
      <=< includeFiles
      .   addSectionLinks
      .   smallCaps
      .   styleLocalLinks
      )
    <=< processBib
 where
  -- Links that go to the site itself get a special CSS class.
  styleLocalLinks :: Pandoc -> Pandoc
  styleLocalLinks = walk \case
    Link (ident, cs, kvs) inlines target@(url, _)
      | any (`T.isPrefixOf` url) [ "/", "./", "../", siteURL ]
     -> Link (ident, "local-link" : cs, kvs) inlines target
    inline -> inline

  -- https://frasertweedale.github.io/blog-fp/posts/2020-12-10-hakyll-section-links.html
  addSectionLinks :: Pandoc -> Pandoc
  addSectionLinks = walk \case
    Header n attr@(idAttr, _, _) inlines ->
      let link = Link ("", ["floatleft", "sec-link"], []) [Str "§"] ("#" <> idAttr, "")
       in Header n attr (inlines <> [link])
    block -> block

  pygmentsHighlight :: Pandoc -> Compiler Pandoc
  pygmentsHighlight = walkM \case
    CodeBlock (_, listToMaybe -> mbLang, _) (T.unpack -> body) -> do
      let lang = T.unpack (fromMaybe "text" mbLang)
      RawBlock "html" . T.pack <$> callPygs lang body
    block -> pure block
   where
    callPygs :: String -> String -> Compiler String
    callPygs lang = unixFilter "pygmentize" [ "-l", lang
                                            , "-f", "html"
                                            , "-P", "cssclass=highlight-" <> lang
                                            , "-P", "cssstyles=padding-left: 1em;"
                                            ]

  includeFiles :: Pandoc -> Compiler Pandoc
  includeFiles = walkM \case
    Div (_, cs, kvs) _ | "include" `elem` cs -> do
      let file = T.unpack
               . fromMaybe (error "includeFiles: no `from` key given!")
               $ lookup "from" kvs
      content <- unsafeCompiler $ T.readFile file
      -- Give the file it's correct code block header if it's code. Note that
      -- I'm *not* using Skylighting for the actual highlighting part; see
      -- 'pygmentsHighlight'.
      pure case listToMaybe (syntaxesByFilename defaultSyntaxMap file) of
        Nothing                  -> Div ("", ["included"], []) [Para [Str content]]
        Just (sName -> "Default") -> Div ("", ["included"], []) [Para [Str content]]
        Just (sName -> l)         -> CodeBlock ("", [l], []) content
    block -> pure block

  processBib :: Item Pandoc -> Compiler (Item Pandoc)
  processBib pandoc = do
    -- See @Citations@ in the main function.
    csl <- load @CSL    "bib/style.csl"
    bib <- load @Biblio "bib/bibliography.bib"
    -- We do want to link citations.
    p <- withItemBody (pure . setMeta "link-citations" True) pandoc
    fmap (tableiseBib . insertRefHeading) <$> processPandocBiblio csl bib p
   where
    -- Insert a heading for the citations.
    insertRefHeading :: Pandoc -> Pandoc
    insertRefHeading = walk $ concatMap \case
      d@(Div ("refs", _, _) _) -> [Header 1 ("references", [], []) [Str "References"], d]
      block                    -> [block]

    -- Arrange all citations in a table, so that they are nicely aligned.
    -- This probably only works with label or numerical styles.
    tableiseBib :: Pandoc -> Pandoc
    tableiseBib = walk \case
      Div a@("refs", _, _) body -> Div a (Many.toList (simpleTable [] (map citToRow body)))
      body                      -> body
     where
      citToRow :: Block -> [Many Block]
      citToRow = map Many.singleton . \case
        Div attr [Para (s1 : ss)] -> [Div attr [Plain [s1]], Plain [Space], Plain ss]
        d                         -> error $ "citToRow: unexpected citation format: " <> show d

  -- This is very manual, but for now that's "good enough".
  smallCaps :: Pandoc -> Pandoc
  smallCaps = walk \case
    Str t -> RawInline "html"
           $ replaceSpecial
           $ foldl' (replace T.toLower)
                    t
                    [ "HTML", "CSS", "GNU", "MELPA", "ELPA", "FLOSS", "AST"
                    , "KDE", "XML", "CLI", "QMK", "GHC", "PDF", "GMM", "QGS"
                    , "PSSL", "TODO", "EDSL", "DSL", "API", "BCQT", "LOWER"
                    , "RAISE", "ADJUST", "TL;DR", "BOX", "PBT", "XDA", "GTK"
                    , "HATC", "CSL", "BY-SA", "TOC", "CT23", "README", "LSP"
                    , "PR", "GIF", "XOR", "TUXEDO", "PNG", "SVG", "CDN"
                    ]
    inline -> inline
   where
    replace :: (Text -> Text) -> Text -> Text -> Text
    replace trans haystack needle = T.replace needle (sc (trans needle)) haystack

    sc :: Text -> Text
    sc s = "<span class=\"small-caps\">" <> s <> "</span>"

    -- Big hack, in order to replace e.g. "XMonad." and "XMonad," with
    -- its small-caps variants, but leave things like
    -- "XMonad.Prompt.OrgMode" alone.
    replaceSpecial :: Text -> Text
    replaceSpecial = zbmath . orcid . go "KMonad" . go "XMonad"
     where
      orcid, zbmath :: Text -> Text
      orcid  s = if s == "orcid"  then sc "orc" <> "i" <> sc "d" else s
      zbmath s = if s == "zbmath" then "zb" <> sc "math"         else s

      go :: Text -> Text -> Text
      go pfx s = if pfx `T.isPrefixOf` s && T.length s < 9
                 then sc (T.toLower (T.take 2 pfx)) <> T.drop 2 s
                 else s

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

-----------------------------------------------------------------------
-- Redirects

redirects :: [(Identifier, String)]
redirects =
    [ -- We used to have the date in the title, as well as extra
      -- directories for individual posts; not anymore!
      ("posts/phd-workflow/2022-05-01-my-phd-workflow.html", "../my-phd-workflow.html")
    , ("posts/2022-05-25-calling-emacs-from-xmonad.html", "calling-emacs-from-xmonad.html")
    , ("posts/query-replace/2022-08-06-query-replace-many.html", "../query-replace-many.html")
    , ("posts/orgmode-prompt/2022-08-27-xmonad-and-org-mode.html", "../xmonad-and-org-mode.html")
    , ("posts/topic-space/2022-09-11-topic-spaces.html", "../topic-spaces.html")
    , ("posts/weighted-colimits/2022-10-15-weighted-colimits.html", "../weighted-colimits.html")
    , ("posts/2022-10-22-emacs-potpourri.html", "emacs-potpourri.html")
    , ("posts/2022-11-05-vertical-previews.html", "vertical-previews.html")
    , ("posts/2022-11-30-package-vc-install.html", "package-vc-install.html")
    , ("posts/2022-12-22-vc-use-package.html", "vc-use-package.html")
    , ("posts/2023-01-10-duality-in-monoidal-categories.html", "duality-in-monoidal-categories.html")
    , ("posts/2023-01-14-orgmode-refiling.html", "orgmode-refiling.html")
    , ("posts/2023-01-21-pygmentising-hakyll.html", "pygmentising-hakyll.html")
    , ("posts/2023-01-27-block-sidenotes.html", "block-sidenotes.html")
    -- The previous state of things was quite horrible, so people may
    -- have been confused.
    , ("posts/2022-05-01-my-phd-workflow.html", "my-phd-workflow.html")
    , ("posts/2022-08-06-query-replace-many.html", "query-replace-many.html")
    , ("posts/2022-08-27-xmonad-and-org-mode.html", "xmonad-and-org-mode.html")
    , ("posts/2022-09-11-topic-spaces.html", "topic-spaces.html")
    , ("posts/2022-10-15-weighted-colimits.html", "weighted-colimits.html")
    ]
