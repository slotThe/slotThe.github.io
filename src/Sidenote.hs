{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE DerivingStrategies       #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{- |
   Module      : Sidenote
   Description : Convert pandoc footnotes to sidenotes
   Copyright   : (c) Tony Zorman, 2023
   License     : GPL-3
   Maintainer  : Tony Zorman <soliditsallgood@mailbox.org>

Heavily inspired by
<https://github.com/jez/pandoc-sidenote/ pandoc-sidenote>,
licensed under MIT.
-}
module Sidenote (usingSidenotes) where

import Control.Monad.State (State, foldM, get, modify', runState)
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import Hakyll (Item (..), writePandocWith)
import Text.Pandoc.Definition (Block (..), Inline (..), Pandoc (..))
import Text.Pandoc.Options (WriterOptions)
import Text.Pandoc.Shared (tshow)
import Text.Pandoc.Walk (walkM)

type NoteType :: Type
data NoteType = Sidenote | Marginnote
  deriving stock (Show, Eq)

type SidenoteState :: Type
data SidenoteState = SNS
  { _writer :: !WriterOptions
  , counter :: !Int
  }

type Sidenote :: Type -> Type
type Sidenote = State SidenoteState

usingSidenotes :: WriterOptions -> Pandoc -> Pandoc
usingSidenotes writer (Pandoc meta blocks) =
  Pandoc meta (walkBlocks (SNS writer 0) blocks)
 where
  walkBlocks :: SidenoteState -> [Block] -> [Block]
  walkBlocks sns = \case
    []       -> []
    (b : bs) -> b' <> walkBlocks s' bs
     where (b', s') = walkM mkSidenote [b] `runState` sns

-- Sidenotes can probably appear in more places; this should be
-- filled-in at some point.
mkSidenote :: [Block] -> Sidenote [Block]
mkSidenote = foldM (\acc b -> (acc <>) <$> single b) []
 where
  -- Try to find and render a sidenote in a single block.
  single :: Block -> Sidenote [Block]
  single = \case
    -- Simulate a paragraph by inserting a dummy block; this is needed
    -- in case two consecutive paragraphs have sidenotes, or a paragraph
    -- doesn't have one at all.
    Para inlines         -> (Para [Str ""] :) <$> renderSidenote [] inlines
    OrderedList attrs bs -> (:[]) . OrderedList attrs <$> traverse mkSidenote bs
    BulletList        bs -> (:[]) . BulletList        <$> traverse mkSidenote bs
    block                -> pure [block]

renderSidenote :: [Inline] -> [Inline] -> Sidenote [Block]
renderSidenote !inlines = \case
  []           -> pure [plain inlines]
  Note bs : xs -> do block <- go bs
                     mappend [plain (commentStart : inlines), block]
                         <$> renderSidenote [] xs
  b       : xs -> renderSidenote (b : inlines) xs
 where
  go :: [Block] -> Sidenote Block
  go blocks = do
    SNS w i <- get <* modify' (\sns -> sns{ counter = 1 + counter sns })
    let (typ, noteText) = getNoteType (render w blocks)
    pure . RawBlock "html" $
      mconcat [ commentEnd     -- See [Note Comment]
              , label typ i <> input i <> note typ noteText
              ]

  getNoteType :: Text -> (NoteType, Text)
  getNoteType t
    | "{-} " `T.isPrefixOf` t = (Marginnote, T.drop 4 t)
    | otherwise               = (Sidenote  , t)

  render :: WriterOptions -> [Block] -> Text
  render w = T.pack . drop 1 . dropWhile (/= '\n') . itemBody -- drop <p></p>\n
           . writePandocWith w . Item "" . Pandoc mempty      -- render

  commentEnd :: T.Text
  commentEnd   = "-->"

  commentStart :: Inline
  commentStart = RawInline "html" "<!--"

  plain :: [Inline] -> Block
  plain = Plain . reverse

-- Extracted from @sidenotes.css@.

label :: NoteType -> Int -> Text
label nt i = "<label for=\"sn-" <> tshow i <> "\" class=\"margin-toggle" <> sidenoteNumber <> "\">" <> altSymbol <> "</label>"
 where
  sidenoteNumber :: Text = case nt of
    Sidenote   -> " sidenote-number"
    Marginnote -> ""
  altSymbol :: Text = case nt of
    Sidenote   -> ""
    Marginnote -> "&#8853;"

input :: Int -> Text
input i = "<input type=\"checkbox\" id=\"sn-" <> tshow i <> "\" class=\"margin-toggle\"/>"

note :: NoteType -> Text -> Text
note nt body = "<div class=\"" <> T.toLower (tshow nt) <> "\">" <> body <> "</div>"

{- [Note Comment]

This is obviously horrible, but we have to do this in order for the
block (which is now not an inline element anymore!) immediately before
the sidenote to be "glued" to the sidenote itself.  In this way, the
number indicating the sidenote does not have an extra space associated
to it, which it otherwise would have.

-}
