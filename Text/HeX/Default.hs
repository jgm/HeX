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
                , command
                , module Text.HeX
                )
where
import Text.Parsec
import Text.HeX
import Text.Blaze.Builder.Core

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

