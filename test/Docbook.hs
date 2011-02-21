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
  addParser Normal $ basicParsers ch
  MathML.defaults
  register "emph" emph
  register "strong" strong
  register "section" (section 1)
  register "subsection" (section 2)
  register "subsubsection" (section 3)
  register "paragraph" (section 4)
  register "subparagraph" (section 5)

emph :: Doc -> Doc
emph arg  = inTags "emphasis" [] arg

strong :: Doc -> Doc
strong arg  = inTags "emphasis" [("role","strong")] arg

section :: Int -> Doc -> HeX Doc
section lev d = do
  num <- getSectionNum lev
  let sectionCmd 1 = "section"
      sectionCmd 2 = "subsection"
      sectionCmd 3 = "subsubsection"
      sectionCmd 4 = "paragraph"
      sectionCmd 5 = "subparagraph"
      sectionCmd _ = "subsubparagraph"
  st <- getState
  let remapCmd n = register (sectionCmd n) $
                     do guard False
                        section n mempty
  let unRemapCmd n = let old = case M.lookup (sectionCmd n) (hexCommands st) of
                                  Just x  -> x
                                  Nothing -> error "Something happened"
                     in  register (sectionCmd n) old
  forM_ [1..lev] remapCmd
  contents <- many getNext
  forM_ [1..lev] unRemapCmd
  return $ (inTags "section" []
            $ "\n" +++ inTags "title" [] (raws num +++ ". " +++ d) +++
              "\n" +++ mconcat contents) +++ "\n"
