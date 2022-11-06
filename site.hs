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
          >>= mkTeaserSnapshot             -- For the previews on the main page.
          >>= loadAndApplyTemplate "templates/post.html"    tagCtx
          >>= saveSnapshot "post-content"  -- For atom feed.
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls

  --- Lists of posts
  -- For showing all posts, we want a list of all posts, followed by a
  -- list of tags with associated posts.
  -- https://stackoverflow.com/questions/52805193/in-hakyll-how-can-i-generate-a-tags-page
  let getAllTags :: Compiler [Item (String, [Identifier])]
      getAllTags = pure $ map (\tgs@(s, _) -> Item (tagsMakeId s) tgs) tagsMap
      mkList :: Context String -> Identifier -> Rules ()
      mkList = mkPostList getAllTags
  -- All posts
  create ["posts.html"] do
    mkList (constField "title" "All posts") "templates/all-posts.html"
  -- Only posts tagged by a certain tag
  tagsRules tags \tag taggedPosts -> do
    let getPosts :: Context b
        getPosts = listField "posts" postCtx (recentFirst =<< loadAll taggedPosts)
    mkList (getPosts <> constField "title" ("Posts tagged \"" ++ tag ++ "\""))
           "templates/post-list.html"

listTagsCtx :: Context (String, [Identifier])
listTagsCtx
   = listFieldWith "tag-posts" postCtx (recentFirst <=< (traverse load . snd . itemBody))
  <> metadataField
  <> titleField "title"

defaultCtxWithTags :: Compiler [Item (String, [Identifier])] -> Context String
defaultCtxWithTags getTags
   = listField "tags"  listTagsCtx getTags
  <> listField "posts" postCtx     (recentFirst =<< loadAll allPosts)
  <> defaultContext

hasTag :: MonadMetadata f => String -> Item a -> f Bool
hasTag tg = fmap (tg `elem`) . getTags . itemIdentifier

config :: Configuration
config = defaultConfiguration{ destinationDirectory = "docs" }

allPosts :: Pattern
allPosts = "posts/**.md"

postCtx :: Context String
postCtx = dateField "date" "%F" <> defaultContext

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
  { feedTitle       = "Tony Zorman – Blog"
  , feedDescription = "Maths, Haskell, Emacs, and whatever else comes to mind."
  , feedAuthorName  = "Tony Zorman"
  , feedAuthorEmail = "tonyzorman@mailbox.org"
  , feedRoot        = "https://tony-zorman.com"
  }

-- | Create a post list, filtering all posts according to @ptn@.
mkPostList :: Compiler [Item (String, [Identifier])] -> Context String -> Identifier -> Rules ()
mkPostList tags ctx template = do
  let ctx' = ctx <> defaultCtxWithTags tags
  route idRoute
  compile $ makeItem ""
        >>= loadAndApplyTemplate template                 ctx'
        >>= loadAndApplyTemplate "templates/default.html" ctx'
        >>= relativizeUrls

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

-- | Used for creating teasers, so the TOC doesn't show on the front
-- page.
suppressToc :: Item String -> Item String
suppressToc = fmap (withTagList suppressor)
 where
  suppressor :: [Tag String] -> [Tag String]
  suppressor tags = pre <> post
   where
    (pre, (_, post)) = fmap (fmap (drop 1) . break (== TagClose "div"))
                     . break (== TagOpen "div" [("id", "contents")])
                     $ tags

mkTeaserSnapshot ::  Item String -> Compiler (Item String)
mkTeaserSnapshot item = item <$ saveSnapshot "post-teaser" (suppressToc item)

-- | In the spirit of the stimmung-themes:
--       https://github.com/motform/stimmung-themes
highlightTheme :: Style
highlightTheme = monochrome
  { tokenStyles
      = Ext.fromList
          [ (CommentTok , defStyle{ tokenColor  = color 0x505050
                                  , tokenItalic = True
                                  })
          , (DataTypeTok, defStyle{ tokenBackground = color 0xf8edff })
          , (StringTok  , defStyle{ tokenBackground = color 0xf2f2f2 })
          , (OperatorTok, defStyle{ tokenBold = True })
          , (OtherTok   , defStyle{ tokenBold = True })
          ]
     <> tokenStyles monochrome
  }
 where
  color :: Int -> Maybe Color
  color = toColor
