{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ViewPatterns        #-}

import Data.Text qualified as T

import Control.Monad
import Data.Maybe
import Hakyll
import Sidenote (usingSidenotes)
import Text.HTML.TagSoup (Tag (TagClose, TagOpen), (~==))
import Text.Pandoc.Definition (Block (Header, CodeBlock, RawBlock), Inline (Link, Str), Pandoc)
import Text.Pandoc.Options
import Text.Pandoc.Templates (compileTemplate)
import Text.Pandoc.Walk (walk, walkM)

main :: IO ()
main = hakyllWith config do
  --- Housekeeping
  match "templates/*" $ compile templateBodyCompiler

  match "css/*" do
    route   idRoute
    compile compressCssCompiler

  match ("posts/**.gif" .||. "posts/**.png" .||. "posts/**.jpg" .||. "images/**") do
    route   idRoute
    compile copyFileCompiler

  match "talks/**.pdf" do
    route   idRoute
    compile copyFileCompiler

  --- Generate tags
  tags@Tags{ tagsMap , tagsMakeId } <- buildTags "posts/**" (fromCapture "tags/**.html")
  let tagCtx = tagsField "tags" tags <> postCtx

  --- RSS
  -- For all posts
  create ["atom.xml"] do        -- Atom feed; see 'feedConfig' below.
    route idRoute
    compile do
      lastPosts <- recentFirst =<< loadAllSnapshots allPosts "post-content"
      renderAtom feedConfig (postCtx <> bodyField "description") lastPosts
  -- For individual tags
  tagsRules tags $ \tag taggedPosts ->
    create [fromFilePath $ "atom-" <> tag <> ".xml"] do
      route idRoute
      compile do
        lastPosts <- recentFirst =<< loadAllSnapshots taggedPosts "post-content"
        renderAtom feedConfig (postCtx <> bodyField "description") lastPosts

  --- The "landing page"
  match "index.html" do
    route idRoute
    compile do
      posts <- recentFirst =<< loadAllSnapshots allPosts "post-teaser"
      let teaserCtx = teaserField "teaser" "post-teaser" <> tagCtx
          indexCtx  = listField "posts" teaserCtx (pure posts) <> defaultContext
      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  --- Sidebar "about"ish sites.
  match (fromList ["research.md", "free-software.md"]) do
    route $ setExtension "html"
    compile $ do
      tocCtx <- getTocCtx defaultContext
      myPandocCompiler
        >>= loadAndApplyTemplate "templates/toc.html"     tocCtx
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  match (fromList ["about.md", "impressum.md"]) do
    route   $ setExtension "html"
    compile $ pandocCompilerNoToc
          >>= loadAndApplyTemplate
                "templates/default.html"
                (  boolField "noindex" (pure True)  -- pls no index
                <> defaultContext )
          >>= relativizeUrls

  --- An individual post
  match allPosts do
    route $ setExtension "html"
    compile $ do
      tocCtx <- getTocCtx tagCtx
      myPandocCompiler
        >>= mkCleanSnapshot "post-teaser"   -- For the previews on the main page.
        >>= loadAndApplyTemplate "templates/post.html"     tocCtx
        >>= mkCleanSnapshot "post-content"  -- For atom feed.
        >>= loadAndApplyTemplate "templates/default.html"  defaultContext
        >>= relativizeUrls

  --- Lists of posts
  -- For showing all posts, we want a list of all posts, followed by a
  -- list of tags with associated posts.
  -- https://stackoverflow.com/questions/52805193/in-hakyll-how-can-i-generate-a-tags-page
  let mkList :: Context String -> String -> String -> Identifier -> Rules ()
      mkList = mkPostList (mkTagAssocs tagsMakeId tagsMap)
  -- All posts
  create ["posts.html"] do
    let allPostsCtx   = listField "posts" postCtx (recentFirst =<< loadAll allPosts)
    mkList allPostsCtx "All posts" "atom" "templates/all-posts.html"
  -- Only posts tagged by a certain tag
  tagsRules tags \tag taggedPosts -> do
    let taggedPostCtx = listField "posts" postCtx (recentFirst =<< loadAll taggedPosts)
    mkList taggedPostCtx
           ("Posts tagged " <> "\"" <> tag <> "\"")
           ("../atom-" <> tag)
           "templates/post-list.html"

-----------------------------------------------------------------------
-- Config

config :: Configuration
config = defaultConfiguration{ destinationDirectory = "docs" }

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
  { feedTitle       = "Tony Zorman – Blog"
  , feedDescription = "Maths, Haskell, Emacs, and whatever else comes to mind."
  , feedAuthorName  = "Tony Zorman"
  , feedAuthorEmail = "tonyzorman@mailbox.org"
  , feedRoot        = "https://tony-zorman.com"
  }

-----------------------------------------------------------------------
-- Contexts

postCtx :: Context String
postCtx = dateField "date" "%F" <> defaultContext

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
-- reasons.  I also want section links as part of the heading (see
-- 'myPandocCompiler'), but these should not show up in the table of
-- contents for obvious reasons.
--
-- Hence, this function (i) creates a writer that has my preferred
-- writer settings, (ii) already renders this with pandoc, and (iii)
-- adds the finished thing as a constant field to the metadata of the
-- current entry.  Additionally, the @no-toc@ option is honoured insofar
-- as a @no-toc@ boolean field is introduced, which can be used from the
-- post template (@.\/templates\/post.html@).
--
-- There is some CSS that makes section links only show on hover in
getTocCtx :: Context a -> Compiler (Context a)
getTocCtx ctx = do
  noToc      <- (Just "true" ==) <$> (getUnderlying >>= (`getMetadataField` "no-toc"))
  writerOpts <- mkTocWriter defaultHakyllWriterOptions
  toc        <- renderPandocWith defaultHakyllReaderOptions writerOpts =<< getResourceBody
  pure $ mconcat [ ctx
                 , constField "toc" (itemBody toc)
                 , if noToc then boolField "no-toc" (pure noToc) else mempty
                 ]
 where
  mkTocWriter :: WriterOptions -> Compiler WriterOptions
  mkTocWriter writerOpts = do
    tmpl <- either (const Nothing) Just <$> unsafeCompiler (compileTemplate "" "$toc$")
    pure $ writerOpts
      { writerTableOfContents = True
      , writerTOCDepth        = 3
      , writerTemplate        = tmpl
      }

-----------------------------------------------------------------------
-- Util

allPosts :: Pattern
allPosts = "posts/**.md"

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
  noPilcrow  = killTags (~== TagOpen ("a" :: String) [("id", "sec-link")]) (== TagClose "a")
  supressToc = killTags (==  TagOpen "div"           [("id", "contents")]) (== TagClose "div")

-- | Find @open@ and kill everything between it and @close@.
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

-- | Emphasise that the default pandoc compiler does not have a TOC.
pandocCompilerNoToc :: Compiler (Item String)
pandocCompilerNoToc = pandocCompiler

-- | Pandoc compiler with syntax highlighting (via @pygmentize@),
-- sidenotes instead of footnotes (see @css/sidenotes.css@ and
-- @src/Sidenote.hs@), and section links.  Also see 'getTocCtx' for more
-- table of contents related things, and @./build.sh@ for
-- LaTeX-rendering.
myPandocCompiler :: Compiler (Item String)
myPandocCompiler =
  pandocCompilerWithTransformM
    defaultHakyllReaderOptions
    myWriter
    (pure . usingSidenotes myWriter <=< pygmentsHighlight  . addSectionLinks)
 where
  myWriter :: WriterOptions
  myWriter = defaultHakyllWriterOptions{ writerHTMLMathMethod = MathJax "" }

  -- https://frasertweedale.github.io/blog-fp/posts/2020-12-10-hakyll-section-links.html
  addSectionLinks :: Pandoc -> Pandoc
  addSectionLinks = walk \case
    Header n attr@(idAttr, _, _) inlines ->
      let link = Link ("sec-link", ["floatright"], []) [Str "¶"] ("#" <> idAttr, "")
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
    callPygs lang = unixFilter "pygmentize" ["-l", lang, "-f", "html"]
