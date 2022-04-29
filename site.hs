{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ImportQualifiedPost #-}

import Data.Text qualified as T

import Control.Arrow
import Data.Monoid
import Data.Text (Text)
import Hakyll
import Text.Pandoc.Options
import Text.Pandoc.Templates (compileTemplate)

main :: IO ()
main = hakyllWith config do
  match "css/*" do
    route   idRoute
    compile compressCssCompiler

  match ("posts/**.gif" .||. "posts/**.png") do
    route   idRoute
    compile copyFileCompiler

  match "talks/*" do
    route   idRoute
    compile copyFileCompiler

  match (fromList ["about.md", "impressum.md"]) do
    route   $ setExtension "html"
    compile $ pandocCompiler      -- no toc
          >>= loadAndApplyTemplate
                "templates/default.html"
                (boolField "noindex" (pure True) <> defaultContext)
          >>= relativizeUrls

  match (fromList ["research.md", "free-software.md"]) do
    route $ setExtension "html"
    compile $ tocPandocCompiler   -- with toc
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls

  match allPosts do
    route $ setExtension "html"
    compile $ tocPandocCompiler
          >>= loadAndApplyTemplate "templates/post.html"    postCtx
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls

  create ["posts.html"] do
    route idRoute
    compile do
      posts <- recentFirst =<< loadAll allPosts
      let allPostsCtx = listField "posts" postCtx (return posts)
                     <> constField "title" "All Posts"
                     <> defaultContext
      makeItem ""
          >>= loadAndApplyTemplate "templates/all-posts.html" allPostsCtx
          >>= loadAndApplyTemplate "templates/default.html"   allPostsCtx
          >>= relativizeUrls

  match "index.html" do
    route idRoute
    compile do
      posts <- recentFirst =<< loadAll allPosts
      let indexCtx = listField "posts" postCtx (return posts)
                  <> defaultContext
      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler

config :: Configuration
config = defaultConfiguration{ destinationDirectory = "docs" }

allPosts :: Pattern
allPosts = "posts/**.md"

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y"
       <> defaultContext

tocPandocCompiler :: Compiler (Item String)
tocPandocCompiler = pandocCompilerWith defaultHakyllReaderOptions
                =<< tocPandocWriterOptions
 where
  tocPandocWriterOptions :: Compiler WriterOptions
  tocPandocWriterOptions = do
    tmpl <- either (const Nothing) Just
        <$> unsafeCompiler (compileTemplate "" tmplSpec)
    pure defaultHakyllWriterOptions
        { writerTableOfContents = True
        , writerTemplate        = tmpl
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
