{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Text.HeX.Standard.LaTeX (commands) where

import Text.HeX
import Text.HeX.Standard.TeX (ctl, ch, grp)
import Text.HeX.Standard.Generic (getSectionNum)

commands :: HeX ()
commands = do
  registerEscaperFor "latex" (return . ch)
  registerFor "latex" "emph" emph
  registerFor "latex" "strong" strong
  registerFor "latex" "section" (section 1)
  registerFor "latex" "subsection" (section 2)
  registerFor "latex" "subsubsection" (section 3)
  registerFor "latex" "paragraph" (section 4)
  registerFor "latex" "subparagraph" (section 5)

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
