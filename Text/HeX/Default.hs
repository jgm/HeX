{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, PatternGuards #-}
{- |
   Module      : Text.HeX.Default
   Copyright   : Copyright (C) 2010 John MacFarlane
   License     : BSD3
   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha

A flexible text macro system. Users can define their own
TeX-like macros (with arbitrary syntax).  Multiple output
formats can be supported by a single set of macros.
-}

module Text.HeX.Default
                ( getOpt
                , oneChar
                , command
                , group
                , math
                , module Text.HeX
                )
where
import Text.Parsec
import Text.HeX
import qualified Text.HeX.Html as Html
import qualified Text.HeX.TeX as TeX
import Text.Blaze.Builder.Core
import Data.Typeable ()
import Control.Monad

readM :: (Monad m, Read a) => String -> m a
readM s | [x] <- parsed = return x
        | otherwise     = fail $ "Failed to parse `" ++ s ++ "'"
  where
    parsed = [x | (x,_) <- reads s]

getOpt :: Read a => HeX a
getOpt = try $ do
  char '['
  res <- manyTill (noneOf "]\n" <|> (char '\\' >> anyChar)) (char ']')
  readM res

command :: String -> HeX Builder -> HeX Builder
command name p = do
  try $ char '\\' >> string name >> notFollowedBy letter
  p

oneChar :: HeX Builder
oneChar = do
  mathmode <- liftM hexMath getState
  c <- (char '\\' >> anyChar) <|> anyChar
  "html"  ==> Html.ch c
   & "tex" ==> if mathmode
                  then fromChar c
                  else TeX.ch c

group :: HeX Builder
group = do
  char '{'
  res <- manyTill getNext (char '}')
  return $ mconcat res

math :: HeX Builder
math = do
  a <- char '$'
  b <- option "" $ count 1 $ char '$'
  let delim = a:b
  updateState $ \s -> s{ hexMath = True }
  raw <- liftM mconcat $ manyTill oneChar (try $ string delim)
  updateState $ \s -> s { hexMath = False }
  let tagtype = if delim == "$" then "span" else "div"
  "html" ==> Html.inTags tagtype [("class","math")] raw
   & "tex" ==> fromString delim +++ raw +++ fromString delim

