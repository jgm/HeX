{-# LANGUAGE OverloadedStrings, PatternGuards,
    FlexibleInstances, TypeSynonymInstances #-}
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
import Control.Monad

readM :: (Read a, Monad m) => String -> m a
readM s | [x] <- parsed = return x
        | otherwise     = fail $ "Failed to parse `" ++ s ++ "'"
  where
    parsed = [x | (x,_) <- reads s]

getOpt :: Read a => HeX (Maybe a)
getOpt = try $ do
  char '['
  liftM (readM . U.toString . renderBS . cat) $ manyTill oneChar (char ']')

blankline :: HeX Char
blankline = try $ many (oneOf " \t") >> newline

skipBlank :: HeX ()
skipBlank = do many $ (newline >> notFollowedBy blankline >> return "\n") <|>
                      (many1 (oneOf " \t")) <|>
                      (char '%' >> manyTill anyChar newline)
               return ()

class ToCommand a where
  toCommand :: a -> HeX Doc

instance ToCommand (HeX Doc) where
  toCommand x = x

instance ToCommand (Format -> HeX Doc) where
  toCommand x = do format <- liftM hexFormat getState
                   x format

instance ToCommand b => ToCommand (Doc -> b) where
  toCommand x = do arg <- getNext
                   toCommand (x arg)

instance (Read a, ToCommand b) => ToCommand (Maybe a -> b) where
  toCommand x = do opt <- getOpt
                   toCommand (x opt)

command :: ToCommand a => String -> a -> HeX Doc
command name x = try $ do
  char '\\'
  string name
  notFollowedBy letter
  skipBlank
  toCommand x

getFormat :: HeX Format
getFormat = liftM hexFormat getState

oneChar :: HeX Doc
oneChar = do
  mathmode <- liftM hexMath getState
  c <- (char '\\' >> anyChar) <|> anyChar
  format <- getFormat
  case format of
       "html" -> return $ Html.ch c
       "tex"  -> if mathmode
                  then return $ rawc c
                  else return $ TeX.ch c
       _      -> fail $ "oneChar: don't know how to handle " ++ format

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
  format <- getFormat
  case format of
       "html" -> return $ Html.inTags tagtype [("class","math")] b
       "tex"  -> return $ raws delim +++ b +++ raws delim
       _      -> fail $ "emitMath: don't know how to handle " ++ format

math :: HeX Doc
math = do
  char '$'
  display <- option False $ char '$' >> return True
  let delim = if display then try (string "$$") else count 1 (char '$')
  raw <- inMathMode $ manyTill getNext delim
  emitMath display $ cat raw

ensureMath :: HeX Doc -> HeX Doc
ensureMath p = do
  mathmode <- liftM hexMath getState
  res <- inMathMode p
  if mathmode
     then return res
     else emitMath False res
