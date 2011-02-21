{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Text.HeX.Standard.Html (defaults) where

import Text.HeX
import Text.HeX.Standard.Xml (ch, inTags)
import Text.HeX.Standard.Generic (getSectionNum)

defaults :: HeX ()
defaults = do
  addParser Normal $ basicParsers ch
  addParser Normal smart
  register "emph" $ inTags "em" []
  register "strong" $ inTags "strong" []
  register "section" (section 1)
  register "subsection" (section 2)
  register "subsubsection" (section 3)
  register "paragraph" (section 4)
  register "subparagraph" (section 5)

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
