{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, PackageImports #-}
module Text.HeX.Standard.Generic (defaults, getSectionNum, para) where

import Text.HeX
import Data.List (intercalate)
import "mtl" Control.Monad.Trans (liftIO)

defaults :: HeX ()
defaults = do
  register [Block,Inline,Math] "rpt" rpt
  register [Block] "include" include
  register [Inline] "label" label'
  register [Inline] "ref" ref

rpt :: Maybe Int -> Doc -> Doc
rpt (Just n) d = mconcat $ replicate n d
rpt Nothing  d = d

include :: FilePath -> HeX Doc
include f = do
  contents <- liftIO $ readFile f
  rest <- getInput
  setInput $ contents ++ rest
  return mempty

getSectionNum :: Int -> HeX String
getSectionNum lev = do
  (secnum :: [Int]) <- getVar "secnum"
  let thisnum = take lev $ zipWith (+) (secnum ++ repeat 0)
                                       (replicate (lev - 1) 0 ++ [1])
  setVar "secnum" thisnum
  let num = intercalate "." $ map show thisnum
  setTarget num
  return num

label' :: String -> HeX Doc
label' s = addLabel s >> return mempty

ref :: String -> HeX Doc
ref s = lookupLabel s

para :: ([Doc] -> Doc) -> HeX Doc
para f = try $ do
  spaces
  notFollowedBy (char '%') -- comment
  res <- withMode Inline $ many1 (try $ notFollowedBy blanklines >> getNext)
  blanklines <|> (spaces >> eof)
  return $ f res

-- | Parses a space or tab.
spaceChar :: HeX Char
spaceChar = satisfy $ \c -> c == ' ' || c == '\t'

-- | Skips zero or more spaces or tabs.
skipSpaces :: HeX ()
skipSpaces = skipMany spaceChar

-- | Skips zero or more spaces or tabs, then reads a newline.
blankline :: HeX ()
blankline = try $ skipSpaces >> newline >> return ()

-- | Parses one or more blank lines and returns a string of newlines.
blanklines :: HeX ()
blanklines =  (try $ skipSpaces >> eof)
          <|> (try $ blankline >> many1 blankline >> return ())

