{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Sidenote
   Description : Convert pandoc footnotes to sidenotes
   Copyright   : (c) Tony Zorman, 2023
   License     : AGPL
   Maintainer  : Tony Zorman <soliditsallgood@mailbox.org>

Heavily inspired by <https://github.com/jez/pandoc-sidenote/ pandoc-sidenote>.
-}
module Sidenote (usingSidenotes) where

import Control.Monad.State
import Data.Text (Text)
import qualified Data.Text as T
import qualified Debug.Trace
import Hakyll
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Walk (walkM)

data SidenoteState = SNS
  { writer  :: !WriterOptions
  , counter :: !Int
  }

type Sidenote = State SidenoteState

usingSidenotes :: WriterOptions -> Pandoc -> Pandoc
usingSidenotes writer (Pandoc meta bs) = Pandoc meta (walkBlocks (SNS writer 0) bs)
 where
  walkBlocks :: SidenoteState -> [Block] -> [Block]
  walkBlocks sns = \case
    []       -> []
    (b : bs) -> b' <> walkBlocks s' bs
     where (b', s') = walkM mkSidenote [b] `runState` sns

-- | Sidenotes can obviously appear in more places—this should be
-- completely filled-in at some point.
mkSidenote :: [Block] -> Sidenote [Block]
mkSidenote = \case
  Para inlines : bs -> do
    noteBlock <- go [] inlines
    -- Simulate a paragraph by inserting a dummy block (this is
    -- important in case two consecutive paragraphs have sidenotes).
    ((Para [Str ""] : noteBlock) <>) <$> mkSidenote bs
  OrderedList attrs blocks : bs -> do
    listBlocks <- traverse mkSidenote blocks
    (OrderedList attrs listBlocks :) <$> mkSidenote bs
  BulletList blocks : bs -> do
    listBlocks <- traverse mkSidenote blocks
    (BulletList listBlocks :) <$> mkSidenote bs
  blocks -> pure blocks
 where
  go :: [Inline] -> [Inline] -> Sidenote [Block]
  go inlines = \case
    []       -> pure [Plain inlines]
    (x : xs) -> case x of
      Note bs -> do block <- renderSidenote bs
                    -- See [Note Comment]
                    ([Plain (inlines |> commentStart), block] <>) <$> go [] xs
      b       -> go (inlines |> b) xs

  containsNote :: [Inline] -> Bool
  containsNote = \case
    []            -> False
    (Note{} : _ ) -> True
    (_      : xs) -> containsNote xs

  (|>) :: [a] -> a -> [a]
  xs |> x = xs <> [x]

renderSidenote :: [Block] -> Sidenote Block
renderSidenote blocks = do
  SNS w i <- get <* modify' (\sns -> sns{ counter = succ (counter sns) })
  let
    renderedBody = T.pack . itemBody $ writePandocWith w (Item "" (Pandoc mempty blocks))
    label = "<label for=\"sn-" <> T.pack (show i)
           <> "\" class=\"margin-toggle sidenote-number\">" <> "</label>"
    input = "<input type=\"checkbox\" id=\"sn-" <> T.pack (show i) <> "\" "
         <> "class=\"margin-toggle\"/>"
    noteBody = "<span class=\"sidenote\">" <> renderedBody <> "</span>"
  pure . RawBlock "html" $
    mconcat [ commentEnd
            , "<span class=\"sidenote-wrapper\">"
            , label <> input <> noteBody
            , "</span>"
            ]

commentStart :: Inline
commentStart = RawInline "html" "<!--"

commentEnd :: T.Text
commentEnd   = "-->"

{- [Note Comment]

This is obviously horrible, but we have to do this in order for the
block (which is now not an inline element anymore!) immediately before
the sidenote to be "glued" to the sidenote itself.  In this way, the
number indicating the sidenote does not have an extra space associated
to it, which it otherwise would have.

-}
