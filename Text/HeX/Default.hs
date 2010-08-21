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
                , ensureMath
                , module Text.HeX
                )
where
import Text.Parsec
import Text.HeX
import qualified Text.HeX.Html as Html
import qualified Text.HeX.TeX as TeX
import Text.Blaze.Builder.Core
import Data.Typeable
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

{-
command :: String -> HeX Builder -> HeX Builder
command name p = do
  try $ char '\\' >> string name >> notFollowedBy letter
  p
-}

command :: Typeable a => a -> HeX Builder
command x = undefined

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

inMathMode :: HeX a -> HeX a
inMathMode p = do
  mathmode <- liftM hexMath getState
  updateState $ \s -> s{ hexMath = True }
  res <- p
  updateState $ \s -> s { hexMath = mathmode }
  return res

emitMath :: Bool -> Builder -> HeX Builder
emitMath display b = do
  let tagtype = if display then "div" else "span"
  let delim = if display then "$$" else "$"
  "html" ==> Html.inTags tagtype [("class","math")] b
   & "tex" ==> fromString delim +++ b +++ fromString delim

math :: HeX Builder
math = do
  char '$'
  display <- option False $ char '$' >> return True
  let delim = if display then try (string "$$") else count 1 (char '$')
  raw <- inMathMode $ manyTill getNext delim
  emitMath display $ mconcat raw

ensureMath :: HeX Builder -> HeX Builder
ensureMath p = do
  mathmode <- liftM hexMath getState
  res <- inMathMode p
  if mathmode
     then return res
     else emitMath False res
