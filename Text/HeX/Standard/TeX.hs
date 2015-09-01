{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Text.HeX.Standard.TeX (ctl,  grp, ch) where

import Text.HeX
import Data.Char (isLetter)

ctl :: String -> Doc
ctl s | all isLetter s = (rawc '\\') +++ (raws s) +++ (rawc ' ')
ctl [c] = (rawc '\\') +++ (rawc c)
ctl s   = error $ "`" ++ s ++ "' is not a valid Doc control sequence"

grp :: [Doc] -> Doc
grp xs = (rawc '{') +++ mconcat xs +++ (rawc '}')

ch :: Char -> Doc
ch c | c `elem` ['$','#','&','%'] = ctl [c]
ch c | c `elem` ['&','~','\\','{','}','_','^'] =
  raws $ "{\\char`\\" ++ [c] ++ "}"
ch c    = rawc c
