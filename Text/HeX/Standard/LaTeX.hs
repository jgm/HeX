{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Text.HeX.Standard.LaTeX (defaults) where

import Text.HeX
import Text.HeX.Standard.TeX (ctl, ch, grp)
import Text.HeX.Standard.Generic (getSectionNum)

defaults :: HeX ()
defaults = do
  addParser Normal $ basicParsers ch
  register "emph" emph
  register "strong" strong
  register "section" (section 1)
  register "subsection" (section 2)
  register "subsubsection" (section 3)
  register "paragraph" (section 4)
  register "subparagraph" (section 5)

emph :: Doc -> Doc
emph arg  = ctl "emph" +++ grp [arg]

strong :: Doc -> Doc
strong arg  = ctl "textbf" +++ grp [arg]

section :: Int -> Doc -> HeX Doc
section lev d = do
  _ <- getSectionNum lev  -- we need to increment the number
  let secheading = case lev of
                        1 -> "section"
                        2 -> "subsection"
                        3 -> "subsubsection"
                        4 -> "paragraph"
                        _ -> "subparagraph"
  return $ ctl secheading +++ grp [d]
