{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Text.HeX.Standard.LaTeX (defaults) where

import Text.HeX
import Text.HeX.Standard.TeX (ctl, ch, grp)
import Text.HeX.Standard.Generic (getSectionNum)

defaults :: HeX ()
defaults = do
  addParser [Inline] $ basicInline ch
  addParser [Block] $ basicBlock toPara
  newCommand [Inline] "emph" emph
  newCommand [Inline] "strong" strong
  newCommand [Block] "section" (section 1)
  newCommand [Block] "subsection" (section 2)
  newCommand [Block] "subsubsection" (section 3)
  newCommand [Block] "paragraph" (section 4)
  newCommand [Block] "subparagraph" (section 5)

toPara :: [Doc] -> Doc
toPara xs = mconcat xs +++ "\n\n"

emph :: InlineDoc -> Doc
emph (InlineDoc arg)  = ctl "emph" +++ grp [arg]

strong :: InlineDoc -> Doc
strong (InlineDoc arg)  = ctl "textbf" +++ grp [arg]

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
