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
ctl s | all isLetter s = (fromChar '\\') `mappend` (fromString s)
                          `mappend` (fromChar ' ')
ctl [c] = (fromChar '\\') `mappend` (fromChar c)
ctl s   = error $ "`" ++ s ++ "' is not a valid Builder control sequence"

str :: String -> Builder
str = mconcat . map ch 

grp :: [Builder] -> Builder
grp xs = (fromChar '{') `mappend` mconcat xs `mappend` (fromChar '}')

parbreak :: Builder
parbreak = fromString "\n\n"

ch :: Char -> Builder
ch c | c `elem` "$#&%" = ctl [c]
ch c | c `elem` "&~\\{}_^"= fromString $ "{\\char`\\" ++ [c] ++ "}"
ch c    = fromChar c

