{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}

import Data.Text qualified as T
import GHC.Exts  qualified as Ext

import Control.Arrow (first)
import Control.Monad
import Data.Foldable
import Data.Text (Text)
import Hakyll
import Skylighting.Types hiding (Item, Context)
import Text.HTML.TagSoup (Tag (TagClose, TagOpen))
import Text.Pandoc.Highlighting
import Text.Pandoc.Options
import Text.Pandoc.Templates (compileTemplate)

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

  match "talks/*" do
    route   idRoute
    compile copyFileCompiler

  -- Syntax highlighting; see below
  create ["css/syntax.css"] do
    route idRoute
    compile . makeItem $ styleToCss highlightTheme

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
    compile $ myPandocCompiler
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
    compile $ myPandocCompiler
          >>= mkSnapshotNoToc "post-teaser"   -- For the previews on the main page.
          >>= loadAndApplyTemplate "templates/post.html"    tagCtx
          >>= mkSnapshotNoToc "post-content"  -- For atom feed.
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
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

-----------------------------------------------------------------------
-- Theming

-- | In the spirit of the stimmung-themes:
--       https://github.com/motform/stimmung-themes
highlightTheme :: Style
highlightTheme = monochrome
  { tokenStyles
      = Ext.fromList
          [ (CommentTok , defStyle{ tokenColor  = color 0x505050, tokenItalic = True })
          , (DataTypeTok, defStyle{ tokenBackground = color 0xf8edff })
          , (StringTok  , defStyle{ tokenColor = color 0x505050 })
          , (OperatorTok, defStyle{ tokenBold = True })
          , (OtherTok   , defStyle)
          ]
     <> tokenStyles monochrome
  }
 where
  color :: Int -> Maybe Color
  color = toColor

-----------------------------------------------------------------------
-- Util

hasTag :: MonadMetadata f => String -> Item a -> f Bool
hasTag tg = fmap (tg `elem`) . getTags . itemIdentifier

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
-- the table of contents.
mkSnapshotNoToc ::  String -> Item String -> Compiler (Item String)
mkSnapshotNoToc name item = item <$ saveSnapshot name (withTagList supressToc <$> item)
 where
  supressToc :: [Tag String] -> [Tag String]
  supressToc tags = pre <> post
   where
    (pre, (_, post)) = fmap (fmap (drop 1) . break (== TagClose "div"))
                     . break (== TagOpen "div" [("id", "contents")])
                     $ tags

-----------------------------------------------------------------------
-- Compilers

-- | Emphasise that the default pandoc compiler does not have a TOC.
pandocCompilerNoToc :: Compiler (Item String)
pandocCompilerNoToc = pandocCompiler

-- | Pandoc compiler with syntax highlighting and a table of contents.
--
-- Sources:
--   + syntax highlighting: https://rebeccaskinner.net/posts/2021-01-31-hakyll-syntax-highlighting.html
--   + TOC                : https://svejcar.dev/posts/2019/11/27/table-of-contents-in-hakyll/
--   + TOC Toggling       : https://github.com/duplode/duplode.github.io/blob/sources/src/site.hs
myPandocCompiler :: Compiler (Item String)
myPandocCompiler = do
  tocWriter <- myPandocWriterOptions
  noToc     <- getUnderlying >>= (`getMetadataField` "no-toc")
  pandocCompilerWith defaultHakyllReaderOptions
                     (maybe tocWriter (const defaultHakyllWriterOptions) noToc)
 where
  myPandocWriterOptions :: Compiler WriterOptions
  myPandocWriterOptions = do
    tmpl <- either (const Nothing) Just
        <$> unsafeCompiler (compileTemplate "" tmplSpec)
    pure defaultHakyllWriterOptions
        { writerTableOfContents = True
        , writerTOCDepth        = 3
        , writerTemplate        = tmpl
        , writerHighlightStyle  = Just highlightTheme
        -- LaTeX rendering
        , writerExtensions
             = writerExtensions defaultHakyllWriterOptions
            <> extensionsFromList
                 [ Ext_tex_math_dollars
                 , Ext_tex_math_double_backslash
                 , Ext_latex_macros
                 , Ext_inline_code_attributes
                 ]
        , writerHTMLMathMethod = MathJax ""
        }
   where
    tmplSpec :: Text
    tmplSpec = T.unlines
        [ "<div id=\"contents\">"
        , "<p class=\"mini-header\">Contents</p>"
        , "$toc$"
        , "</div>"
        , "$body$"
        ]
