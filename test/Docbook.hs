{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Docbook (defaults) where

import Text.HeX.Standard.Xml (str, ch, inTags, tagSelfClosing)
import Text.HeX.Standard.Generic (getSectionNum)
import qualified Text.HeX.Math.MathML as MathML
import Text.HeX
import Text.Parsec
import Control.Monad
import qualified Data.Map as M

defaults :: HeX ()
defaults = do
  addParser [Block] $ basicBlock (inTags "para" [] . mconcat)
  addParser [Inline] $ basicInline ch
  MathML.defaults
  newCommand [Inline] "emph" emph
  newCommand [Inline] "strong" strong
  newCommand [Block] "section" (section 1)
  newCommand [Block] "subsection" (section 2)
  newCommand [Block] "subsubsection" (section 3)
  newCommand [Block] "paragraph" (section 4)
  newCommand [Block] "subparagraph" (section 5)

emph :: InlineDoc -> Doc
emph (InlineDoc arg)  = inTags "emphasis" [] arg

strong :: InlineDoc -> Doc
strong (InlineDoc arg)  = inTags "emphasis" [("role","strong")] arg

section :: Int -> InlineDoc -> HeX Doc
section lev (InlineDoc d) = do
  num <- getSectionNum lev
  let sectionCmd 1 = "section"
      sectionCmd 2 = "subsection"
      sectionCmd 3 = "subsubsection"
      sectionCmd 4 = "paragraph"
      sectionCmd 5 = "subparagraph"
      sectionCmd _ = "subsubparagraph"
  st <- getState
  let remapCmd n = newCommand [Block] (sectionCmd n) $
                     do guard False
                        section n mempty
  let unRemapCmd n = let old = case M.lookup (Block, sectionCmd n) (hexCommands st) of
                                  Just x  -> x
                                  Nothing -> error "Something happened"
                     in  newCommand [Block] (sectionCmd n) old
  forM_ [1..lev] remapCmd
  contents <- many block
  forM_ [1..lev] unRemapCmd
  return $ (inTags "section" []
            $ "\n" +++ inTags "title" [] (raws num +++ ". " +++ d) +++
              "\n" +++ mconcat contents) +++ "\n"
