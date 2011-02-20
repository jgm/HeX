{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Text.HeX.Standard.Html (defaults) where

import Text.HeX
import Text.HeX.Standard.Xml (ch, inTags)
import Text.HeX.Standard.Generic (getSectionNum)

defaults :: HeX ()
defaults = do
  registerEscaperFor "html" (return . ch)
  addParser smart
  registerFor "html" "emph" $ inTags "em" []
  registerFor "html" "strong" $ inTags "strong" []
  registerFor "html" "section" (section 1)
  registerFor "html" "subsection" (section 2)
  registerFor "html" "subsubsection" (section 3)
  registerFor "html" "paragraph" (section 4)
  registerFor "html" "subparagraph" (section 5)

smart :: HeX Doc
smart = lquo <|> rquo <|> dash

lquo :: HeX Doc
lquo = do
  char '`'
  option "&lsquo;" (char '`' >> return "&ldquo;")

rquo :: HeX Doc
rquo = do
  char '\''
  option "&rsquo;" (char '\'' >> return "&rdquo;")

dash :: HeX Doc
dash = try $ do
  string "--"
  option "&ndash;" (char '-' >> return "&mdash;")

section :: Int -> Doc -> HeX Doc
section lev d = do
  num <- getSectionNum lev
  return $ inTags ("h" ++ show lev) [] (raws num +++ ". " +++ d)
