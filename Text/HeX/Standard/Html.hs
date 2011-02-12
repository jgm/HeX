{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Text.HeX.Standard.Html (commands) where

import Text.HeX
import Text.HeX.Standard.Generic (getSectionNum)

commands :: HeX ()
commands = do
  registerEscaperFor "html" (return . ch)
  registerEmitMathFor "html" emitMath
  registerFor "html" "emph" emph
  registerFor "html" "strong" strong
  registerFor "html" "section" (section 1)
  registerFor "html" "subsection" (section 2)
  registerFor "html" "subsubsection" (section 3)
  registerFor "html" "paragraph" (section 4)
  registerFor "html" "subparagraph" (section 5)

str :: String -> Doc
str = mconcat . map ch

ch :: Char -> Doc
ch '&' = raws "&amp;"
ch '<' = raws "&lt;"
ch '>' = raws "&gt;"
ch c   = rawc c

tag :: Bool -> String -> [(String, String)] -> Doc
tag selfclosing s attrs = "<" +++ raws s +++ toattrs attrs +++ ending
  where toattrs = mconcat . map toattr
        toattr (k,v) = " " +++ raws k +++ ch '=' +++ ch '"' +++
                        str v +++ ch '"'
        ending = raws $ if selfclosing then " />" else ">"

tagOpen :: String -> [(String, String)] -> Doc
tagOpen = tag False

tagSelfClosing :: String -> [(String, String)] -> Doc
tagSelfClosing = tag True

tagClose :: String -> Doc
tagClose s = "</" +++ raws s +++ ">"

inTags :: String -> [(String, String)] -> Doc -> Doc
inTags s attrs x = tagOpen s attrs +++ x +++ tagClose s

emitMath :: Bool -> Doc -> HeX Doc
emitMath display b = do
  let tagtype = if display then "div" else "span"
  return $ inTags tagtype [("class","math")] b

emph :: Doc -> Doc
emph arg  = inTags "em" [] arg

strong :: Doc -> Doc
strong arg  = inTags "strong" [] arg

section :: Int -> Doc -> HeX Doc
section lev d = do
  num <- getSectionNum lev
  return $ inTags ("h" ++ show lev) [] (raws num +++ ". " +++ d)
