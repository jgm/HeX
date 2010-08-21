module Text.HeX.TeX (
    ctl
  , ch
  , str
  , grp
  , parbreak
  )
where
import Text.Blaze.Builder.Core
import Data.Char
import Data.Monoid
import Text.HeX

ctl :: String -> Builder
ctl s | all isLetter s = (rawc '\\') `mappend` (raws s)
                          `mappend` (rawc ' ')
ctl [c] = (rawc '\\') `mappend` (rawc c)
ctl s   = error $ "`" ++ s ++ "' is not a valid Builder control sequence"

str :: String -> Builder
str = mconcat . map ch 

grp :: [Builder] -> Builder
grp xs = (rawc '{') `mappend` mconcat xs `mappend` (rawc '}')

parbreak :: Builder
parbreak = raws "\n\n"

ch :: Char -> Builder
ch c | c `elem` "$#&%" = ctl [c]
ch c | c `elem` "&~\\{}_^"= raws $ "{\\char`\\" ++ [c] ++ "}"
ch c    = rawc c

