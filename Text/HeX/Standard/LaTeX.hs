{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Text.HeX.Standard.LaTeX (defaults) where

import Text.HeX
import Text.HeX.Standard.TeX (ctl, ch, grp)
import Text.HeX.Standard.Generic (getSectionNum)

defaults :: HeX ()
defaults = do
  addParser [Inline] $ basicInline ch
  addParser [Block] $ basicBlock toPara
  register [Inline] "emph" emph
  register [Inline] "strong" strong
  register [Block] "section" (section 1)
  register [Block] "subsection" (section 2)
  register [Block] "subsubsection" (section 3)
  register [Block] "paragraph" (section 4)
  register [Block] "subparagraph" (section 5)

toPara :: [Doc] -> Doc
toPara xs = mconcat xs +++ "\n\n"

emph :: Doc -> Doc
emph arg  = ctl "emph" +++ grp [arg]

strong :: Doc -> Doc
strong arg  = ctl "textbf" +++ grp [arg]

section :: Int -> InlineDoc -> HeX Doc
section lev (InlineDoc d) = do
  _ <- getSectionNum lev  -- we need to increment the number
  let secheading = case lev of
                        1 -> "section"
                        2 -> "subsection"
                        3 -> "subsubsection"
                        4 -> "paragraph"
                        _ -> "subparagraph"
  return $ ctl secheading +++ grp [d] +++ "\n"
