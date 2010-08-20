{-# LANGUAGE OverloadedStrings #-}
module Text.HeX.Html (
    ch
  , str
  , tag
  , tagSelfClosing
  , inTags
  )
where
import Text.Blaze.Builder.Core
import Text.Blaze.Builder.Utf8
import Data.Monoid
import Text.HeX

str :: String -> Builder
str = mconcat . map ch 

ch :: Char -> Builder
ch '&' = fromString "&amp;"
ch '<' = fromString "&lt;"
ch '>' = fromString "&gt;"
ch c   = fromChar c

tag :: Bool -> String -> [(String, String)] -> Builder
tag selfclosing s attrs = "<" +++ fromString s +++ toattrs attrs +++ ending
  where toattrs = mconcat . map toattr
        toattr (k,v) = " " +++ fromString k +++ ch '=' +++ ch '"' +++
                        str v +++ ch '"'
        ending = fromString $ if selfclosing then " />" else ">"

tagOpen :: String -> [(String, String)] -> Builder
tagOpen = tag False

tagSelfClosing :: String -> [(String, String)] -> Builder
tagSelfClosing = tag True

tagClose :: String -> Builder
tagClose s = "</" +++ fromString s +++ ">"

inTags :: String -> [(String, String)] -> Builder -> Builder
inTags s attrs x = tagOpen s attrs +++ x +++ tagClose s
