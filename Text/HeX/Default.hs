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
                ( oneChar
                , command
                , withOpt
                , withArg
                , group
                , math
                , ensureMath
                , module Text.HeX
                )
where
import Text.Parsec
import Text.HeX
import qualified Data.ByteString.Lazy.UTF8 as U
import qualified Text.HeX.Html as Html
import qualified Text.HeX.TeX as TeX
import Data.Typeable
import Control.Monad

readM :: (Read a, Monad m) => String -> m a
readM s | [x] <- parsed = return x
        | otherwise     = fail $ "Failed to parse `" ++ s ++ "'"
  where
    parsed = [x | (x,_) <- reads s]

getOpt :: HeX Doc
getOpt = try $ do
  char '['
  liftM cat $ manyTill oneChar (char ']')

blankline :: HeX Char
blankline = try $ many (oneOf " \t") >> newline

skipBlank :: HeX ()
skipBlank = do many $ (newline >> notFollowedBy blankline >> return "\n") <|>
                      (many1 (oneOf " \t")) <|>
                      (char '%' >> manyTill anyChar newline)
               return ()

command :: String -> (Format -> HeX Doc) -> HeX Doc
command name x = try $ do
  char '\\'
  string name
  notFollowedBy letter
  skipBlank
  format <- liftM hexFormat getState
  x format

withOpt :: (Read a, Typeable a) 
        => (Format -> Maybe a -> HeX Doc)
        -> Format -> HeX Doc
withOpt x f = getOpt >>= x f . readM . U.toString . renderBS

withArg :: (Format -> Doc -> HeX Doc) -> Format -> HeX Doc
withArg x f = getNext >>= x f

oneChar :: HeX Doc
oneChar = do
  mathmode <- liftM hexMath getState
  c <- (char '\\' >> anyChar) <|> anyChar
  "html"  ==> Html.ch c
   & "tex" ==> if mathmode
                  then rawc c
                  else TeX.ch c

group :: HeX Doc
group = do
  char '{'
  res <- manyTill getNext (char '}')
  return $ cat res

inMathMode :: HeX a -> HeX a
inMathMode p = do
  mathmode <- liftM hexMath getState
  updateState $ \s -> s{ hexMath = True }
  res <- p
  updateState $ \s -> s { hexMath = mathmode }
  return res

emitMath :: Bool -> Doc -> HeX Doc
emitMath display b = do
  let tagtype = if display then "div" else "span"
  let delim = if display then "$$" else "$"
  "html" ==> Html.inTags tagtype [("class","math")] b
   & "tex" ==> raws delim +++ b +++ raws delim

math :: HeX Doc
math = do
  char '$'
  display <- option False $ char '$' >> return True
  let delim = if display then try (string "$$") else count 1 (char '$')
  raw <- inMathMode $ manyTill getNext delim
  emitMath display $ cat raw

ensureMath :: (Format -> HeX Doc) -> Format -> HeX Doc
ensureMath p f = do
  mathmode <- liftM hexMath getState
  res <- inMathMode $ p f
  if mathmode
     then return res
     else emitMath False res
