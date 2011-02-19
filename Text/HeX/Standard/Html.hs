{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Text.HeX.Standard.Html (defaults) where

import Text.HeX
import Text.HeX.Standard.Xml (ch, inTags)
import Text.HeX.Standard.Generic (getSectionNum)

defaults :: HeX ()
defaults = do
  registerEscaperFor "html" (return . ch)
  registerFor "html" "emph" emph
  registerFor "html" "strong" strong
  registerFor "html" "section" (section 1)
  registerFor "html" "subsection" (section 2)
  registerFor "html" "subsubsection" (section 3)
  registerFor "html" "paragraph" (section 4)
  registerFor "html" "subparagraph" (section 5)

emph :: Doc -> Doc
emph arg  = inTags "em" [] arg

strong :: Doc -> Doc
strong arg  = inTags "strong" [] arg

section :: Int -> Doc -> HeX Doc
section lev d = do
  num <- getSectionNum lev
  return $ inTags ("h" ++ show lev) [] (raws num +++ ". " +++ d)
