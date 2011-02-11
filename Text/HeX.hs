{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, PatternGuards,
    TypeSynonymInstances, GeneralizedNewtypeDeriving #-}
{- |
   Module      : Text.HeX
   Copyright   : Copyright (C) 2010 John MacFarlane
   License     : BSD3
   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha

A flexible text macro system. Users can define their own
TeX-like macros (with arbitrary syntax).  Multiple output
formats can be supported by a single set of macros.
-}

module Text.HeX ( run
                , setVar
                , getVar
                , updateVar
                , module Text.HeX.Types
                , module Text.Parsec
                , module Data.Monoid
                , oneChar
                , command
                , addCommand
                , parseDoc
                , math
                , ensureMath
                , defaultMain
                )
where
import Text.HeX.Types
import Text.HeX.TeX as TeX
import Text.HeX.Html as Html
import Text.Parsec
import qualified Data.ByteString.Lazy as L
import System.Environment
import Control.Monad
import Data.Char (toLower, isLetter)
import Data.Dynamic
import Blaze.ByteString.Builder
import qualified Data.Map as M
import Data.Monoid

setVar :: Typeable a => String -> a -> HeX a
setVar name' v = do
  updateState $ \s ->
     s{ hexVars = M.insert name' (toDyn v) $ hexVars s }
  return v

getVar :: Typeable a => String -> HeX a
getVar name' = do
  vars <- liftM hexVars getState
  case M.lookup name' vars of
        Just v   -> case fromDynamic v of
                          Just v' -> return v'
                          Nothing -> fail $ "Variable `" ++ name' ++
                                       "' is of type " ++ show (dynTypeRep v)
        Nothing  -> fail $ "Variable `" ++ name' ++ "' has not been set."

updateVar :: Typeable a => String -> (a -> a) -> HeX a
updateVar name' f = getVar name' >>= setVar name' . f

run :: HeX Doc -> Format -> String -> IO L.ByteString
run parser format contents = do
  result <- runParserT (do res <- parser
                           case res of
                                Doc b  -> return b
                                Fut f  -> getState >>= return . f)
               HeXState{ hexParsers = [math, group, oneChar, command]
                       , hexCommands = M.empty
                       , hexFormat = format
                       , hexMath = False
                       , hexVars = M.empty
                       , hexLabels = M.empty } "input" contents
  case result of
       Left e          -> error (show e)
       Right b         -> return $ toLazyByteString b

blankline :: HeX Char
blankline = try $ many (oneOf " \t") >> newline

skipBlank :: HeX ()
skipBlank = do many $ (newline >> notFollowedBy blankline >> return "\n") <|>
                      (many1 (oneOf " \t")) <|>
                      (char '%' >> manyTill anyChar newline)
               return ()

addCommand :: ToCommand a => String -> a -> HeX ()
addCommand name x = updateState $ \s ->
    s{ hexCommands = M.insert name (toCommand x) (hexCommands s) }

parseDoc :: HeX Doc
parseDoc = do
  results <- spaces >> many getNext
  spaces
  eof
  return $ mconcat results

getFormat :: HeX Format
getFormat = liftM hexFormat getState

oneChar :: HeX Doc
oneChar = try $ do
  mathmode <- liftM hexMath getState
  c <- (try $ char '\\' >> (satisfy (not . isLetter))) <|> satisfy (/='\\')
  format <- getFormat
  case format of
       Html -> return $ Html.ch c
       LaTeX -> if mathmode
                   then return $ rawc c
                   else return $ TeX.ch c

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
       Html  -> return $ Html.inTags tagtype [("class","math")] b
       LaTeX -> return $ raws delim +++ b +++ raws delim

math :: HeX Doc
math = do
  char '$'
  display <- option False $ char '$' >> return True
  let delim = if display then try (string "$$") else count 1 (char '$')
  raw <- inMathMode $ manyTill getNext delim
  emitMath display $ mconcat raw

ensureMath :: HeX Doc -> HeX Doc
ensureMath p = do
  mathmode <- liftM hexMath getState
  res <- inMathMode p
  if mathmode
     then return res
     else emitMath False res

command :: HeX Doc
command = do
  char '\\'
  cmd <- many1 letter
  skipBlank
  commands <- liftM hexCommands getState
  case M.lookup cmd commands of
        Just p  -> p
        Nothing -> fail $ ('\\':cmd) ++ " is not defined"

defaultMain :: HeX Doc -> IO ()
defaultMain parser = do
  inp <- getContents
  args <- getArgs
  when (null args) $ error "Specify output format."
  let format = case map toLower $ head args of
                 "html"  -> Html
                 "latex" -> LaTeX
                 x       -> error $ "Unknown output format: " ++ x
  L.putStrLn =<< run parser format inp


