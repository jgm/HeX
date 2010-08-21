module Text.HeX.TeX (
    ctl
  , ch
  , str
  , grp
  , parbreak
  )
where
import Data.Char
import Text.HeX

ctl :: String -> Doc
ctl s | all isLetter s = (rawc '\\') +++ (raws s) +++ (rawc ' ')
ctl [c] = (rawc '\\') +++ (rawc c)
ctl s   = error $ "`" ++ s ++ "' is not a valid Doc control sequence"

str :: String -> Doc
str = cat . map ch 

grp :: [Doc] -> Doc
grp xs = (rawc '{') +++ cat xs +++ (rawc '}')

parbreak :: Doc
parbreak = raws "\n\n"

ch :: Char -> Doc
ch c | c `elem` "$#&%" = ctl [c]
ch c | c `elem` "&~\\{}_^"= raws $ "{\\char`\\" ++ [c] ++ "}"
ch c    = rawc c

