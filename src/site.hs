{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

import Data.Text qualified as T

import Control.Monad
import Data.List (foldl')
import Data.Maybe
import Data.Text (Text)
import Hakyll
import Text.HTML.TagSoup (Tag (TagClose, TagOpen), (~==))
import Text.Pandoc.Definition (Block (..), Inline (..), Pandoc)
import Text.Pandoc.Options
import Text.Pandoc.Shared (headerShift)
import Text.Pandoc.SideNoteHTML (usingSideNotesHTML)
import Text.Pandoc.Templates (compileTemplate)
import Text.Pandoc.Walk (walk, walkM)

main :: IO ()
main = hakyllWith config do
  --- Housekeeping
  match "templates/*" $ compile templateBodyCompiler

  match "css/*" do
    route   idRoute
    compile compressCssCompiler

  match ("images/**" .||. "talks/**.pdf" .||. "fonts/**") do
    route   idRoute
    compile copyFileCompiler

  --- Redirects
  version "redirects" $ createRedirects redirects

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
          indexCtx :: Context String
                    = listField "posts" teaserCtx (pure posts) <> bodyField "body"
      getResourceBody
        >>= applyAsTemplate                               indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  --- Sidebar "about"ish sites.
  match (fromList ["research.md", "free-software.md"]) do
    route $ setExtension "html"
    compile $ do
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

  --- An individual post
  match allPosts do
    route $ setExtension "html"
    compile $ do
      tocCtx <- getTocCtx tagCtx
      -- Atom feeds get their own compiler, since the website uses a lot
      -- of stuff (sidenotes, small-caps…) that doesn't work in feeds.
      void $ pandocRssCompiler
         >>= loadAndApplyTemplate "templates/post.html"    tocCtx
         >>= mkCleanSnapshot "post-content"  -- For atom feed.
      -- Actual compiler for the website
      myPandocCompiler
        >>= mkCleanSnapshot "post-teaser"   -- For the previews on the main page.
        >>= loadAndApplyTemplate "templates/post.html"     tocCtx
        >>= loadAndApplyTemplate "templates/default.html"  tocCtx
        >>= relativizeUrls

  --- Lists of posts
  -- For showing all posts, we want a list of all posts, followed by a
  -- list of tags with associated posts.
  -- https://stackoverflow.com/questions/52805193/in-hakyll-how-can-i-generate-a-tags-page
  let mkList :: Context String -> String -> String -> Identifier -> Rules ()
      mkList = mkPostList (mkTagAssocs tagsMakeId tagsMap)
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

-----------------------------------------------------------------------
-- Config

config :: Configuration
config = defaultConfiguration{ destinationDirectory = "docs" }

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
  { feedTitle       = "Tony Zorman · Blog"
  , feedDescription = "Maths, Haskell, Emacs, and whatever else comes to mind."
  , feedAuthorName  = "Tony Zorman"
  , feedAuthorEmail = "tonyzorman@mailbox.org"
  , feedRoot        = "https://tony-zorman.com"
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
                 , constField "toc" (killLinkIds (itemBody toc))
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

  -- Pandoc now IDs for its table of contents.[1, 2] However, since the
  -- site has two TOCs (where only one is shown at a given time via
  -- CSS), this results in two elements having the same identifier,
  -- which is invalid HTML.  This may or may not matter, but it's
  -- certainly better to fix it.
  --
  -- [1]: https://github.com/jgm/pandoc/issues/7907
  -- [2]: https://github.com/jgm/pandoc/pull/7913
  killLinkIds :: String -> String
  killLinkIds = T.unpack . mconcat . go . T.splitOn "id=\"toc-" . T.pack
   where
    go :: [Text] -> [Text]
    go = \case
      []     -> []
      x : xs -> x : map (T.drop 1 . T.dropWhile (/= '\"')) xs

-----------------------------------------------------------------------
-- Util

allPosts :: Pattern
allPosts = "posts/**.md"

addTitle :: String -> String -> String
addTitle title body = "<span title=\"" <> title <> "\">" <> body <> "</span>"

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

myWriter :: WriterOptions
myWriter = defaultHakyllWriterOptions{ writerHTMLMathMethod = MathJax "" }

-- | A simple pandoc compiler for RSS/Atom feeds, with none of the
-- fanciness that 'myPandocCompiler' has.
pandocRssCompiler :: Compiler (Item String)
pandocRssCompiler = pandocCompilerWorker pure

pandocCompilerWorker :: (Pandoc -> Compiler Pandoc) -> Compiler (Item String)
pandocCompilerWorker =
  pandocCompilerWithTransformM
    defaultHakyllReaderOptions
    myWriter
    . (. headerShift 1)                   -- only the `title' should be <h1>

-- | Pandoc compiler with syntax highlighting (via @pygmentize@),
-- sidenotes instead of footnotes (see @css/sidenotes.css@ and
-- @src/Sidenote.hs@), automatic small-caps for certain abbreviations,
-- and section links.  Also see 'getTocCtx' for more table of contents
-- related things, and @./build.sh@ for LaTeX-rendering.
myPandocCompiler :: Compiler (Item String)
myPandocCompiler =
  pandocCompilerWorker
    (   pure . usingSideNotesHTML myWriter  -- needs to be last because it renders html
    <=< pygmentsHighlight
    .   addSectionLinks
    .   smallCaps
    )
 where
  -- https://frasertweedale.github.io/blog-fp/posts/2020-12-10-hakyll-section-links.html
  addSectionLinks :: Pandoc -> Pandoc
  addSectionLinks = walk \case
    Header n attr@(idAttr, _, _) inlines ->
      let link = Link ("", ["floatright", "sec-link"], []) [Str "¶"] ("#" <> idAttr, "")
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

  -- This is very manual, but for now that's "good enough".
  smallCaps :: Pandoc -> Pandoc
  smallCaps = walk \case
    Str t -> RawInline "html"
           $ replaceSpecial
           $ foldl' (replace T.toLower)
                    t
                    ["HTML", "CSS", "GNU", "MELPA", "ELPA", "FLOSS", "AST", "KDE", "XML", "CLI", "QMK", "GHC", "PDF", "GMM", "QGS", "PSSL", "TODO", "EDSL", "DSL", "API", "BCQT", "LOWER", "RAISE", "ADJUST", "TL;DR", "BOX", "PBT", "XDA", "GTK"]
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
    replaceSpecial = go "KMonad" . go "XMonad"
     where
      go :: Text -> Text -> Text
      go pfx s = if pfx `T.isPrefixOf` s && T.length s < 9
                 then sc (T.toLower (T.take 2 pfx)) <> T.drop 2 s
                 else s

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
