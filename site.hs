{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ImportQualifiedPost #-}

import Data.Text qualified as T
import GHC.Exts  qualified as Ext

import Data.Text (Text)
import Hakyll
import Skylighting.Types hiding (Context)
import Text.HTML.TagSoup (Tag (TagClose, TagOpen))
import Text.Pandoc.Highlighting
import Text.Pandoc.Options
import Text.Pandoc.Templates (compileTemplate)

main :: IO ()
main = hakyllWith config do
  match "templates/*" $ compile templateBodyCompiler

  tags <- buildTags "posts/**" (fromCapture "tags/**.html")

  match "css/*" do
    route   idRoute
    compile compressCssCompiler

  match ("posts/**.gif" .||. "posts/**.png" .||. "posts/**.jpg" .||. "images/**") do
    route   idRoute
    compile copyFileCompiler

  match "talks/*" do
    route   idRoute
    compile copyFileCompiler

  create ["css/syntax.css"] do  -- Syntax highlighting; see below.
    route idRoute
    compile . makeItem $ styleToCss highlightTheme

  create ["atom.xml"] do        -- Atom feed; see 'feedConfig' below.
    route idRoute
    compile do
      lastPosts <- recentFirst =<< loadAllSnapshots allPosts "post-content"
      renderAtom feedConfig (postCtx <> bodyField "description") lastPosts

  --- The "landing page"
  match "index.html" do
    route idRoute
    compile do
      posts <- recentFirst =<< loadAllSnapshots allPosts "post-teaser"
      let teaserCtx = teaserField "teaser" "post-teaser" <> postCtx
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

  --- Posts
  match allPosts do
    route $ setExtension "html"
    let tagCtx = tagsField "tags" tags <> postCtx
    compile $ myPandocCompiler
          >>= mkTeaserSnapshot             -- For the previews on the main page.
          >>= loadAndApplyTemplate "templates/post.html"    tagCtx
          >>= saveSnapshot "post-content"  -- For atom feed.
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls

  --- Lists of posts
  -- All posts
  create ["posts.html"] $
    mkPostList allPosts (constField "title" "All Posts")

  -- Only posts tagged by a certain tag
  tagsRules tags \tag taggedPosts ->
    mkPostList taggedPosts (constField "title" ("Posts tagged \"" ++ tag ++ "\""))

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
mkPostList :: Pattern -> Context String -> Rules ()
mkPostList ptn ctx = do
  let ctx' = getPosts <> ctx <> defaultContext
  route idRoute
  compile $ makeItem ""
        >>= loadAndApplyTemplate "templates/all-posts.html" ctx'
        >>= loadAndApplyTemplate "templates/default.html"   ctx'
        >>= relativizeUrls
 where
  getPosts :: Context a
  getPosts = listField "posts" postCtx (recentFirst =<< loadAll ptn)

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
