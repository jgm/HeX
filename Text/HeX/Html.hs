module Text.HeX.Html (
    ch
  , str
  )
where
import Text.Blaze.Builder.Core
import Text.Blaze.Builder.Utf8
import Data.Monoid

str :: String -> Builder
str = mconcat . map ch 

ch :: Char -> Builder
ch '&' = fromString "&amp;"
ch '<' = fromString "&lt;"
ch '>' = fromString "&gt;"
ch c   = fromChar c

