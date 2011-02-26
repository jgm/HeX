{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Text.HeX.Standard.Html (defaults) where

import Text.HeX
import Text.HeX.Standard.Xml (ch, inTags)
import Text.HeX.Standard.Generic (getSectionNum, para)

defaults :: HeX ()
defaults = do
  addParser [Inline] $ basicInline ch
  addParser [Inline] smart
  addParser [Block] basicBlock
  addParser [Block] $ para toPara
  register [Inline] "emph" $ inTags "em" []
  register [Inline] "strong" $ inTags "strong" []
  register [Block] "section" (section 1)
  register [Block] "subsection" (section 2)
  register [Block] "subsubsection" (section 3)
  register [Block] "paragraph" (section 4)
  register [Block] "subparagraph" (section 5)

toPara :: [Doc] -> Doc
toPara xs = inTags "p" [] (mconcat xs) +++ "\n"

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
