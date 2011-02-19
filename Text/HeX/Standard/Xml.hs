{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Text.HeX.Standard.Xml (str, ch, tagOpen, tagClose,
                              tagSelfClosing, inTags) where

import Text.HeX

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

