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
                , registerEscaperFor
                , oneChar
                , addParser
                , command
                , parseDoc
                , setTarget
                , addLabel
                , lookupLabel
                , math
                , ensureMath
                , inMathMode
                , registerEmitMathFor
                , defaultMain
                )
where
import Text.HeX.Types
import Text.Parsec
import qualified Data.ByteString.Lazy as L
import Blaze.ByteString.Builder.Char.Utf8 as BU
import System.Environment
import Control.Monad
import Data.Char (isLetter)
import Data.Dynamic
import Blaze.ByteString.Builder
import qualified Data.Map as M
import Data.Monoid
import Data.Maybe (fromMaybe)
import qualified Data.CaseInsensitive as CI

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

setTarget :: String -> HeX ()
setTarget s = updateState $ \st -> st{ hexTarget = s }

addLabel :: String -> HeX ()
addLabel s = do
  st' <- getState
  let target = hexTarget st'
  let labs = hexLabels st'
  guard $ not (null target)
  updateState $ \st -> st{ hexLabels = M.insert s target labs }

lookupLabel :: String -> HeX Doc
lookupLabel lab = return $ Fut $ \st -> do
  let labs = hexLabels st
  return $ BU.fromString $ fromMaybe "??" $ M.lookup lab labs

run :: HeX Doc -> Format -> String -> IO L.ByteString
run parser format contents = do
  result <- runParserT (do res <- parser
                           case res of
                                Doc b  -> return b
                                Fut f  -> getState >>= f)
               HeXState{ hexParsers = [math, group, oneChar, command]
                       , hexEscapers = M.empty
                       , hexEmitMath = M.empty
                       , hexCommands = M.empty
                       , hexFormat = format
                       , hexMath = False
                       , hexVars = M.empty
                       , hexTarget = ""
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

parseDoc :: HeX Doc
parseDoc = do
  results <- spaces >> many getNext
  spaces
  eof
  return $ mconcat results

getFormat :: HeX Format
getFormat = liftM hexFormat getState

registerEscaperFor :: Format -> (Char -> HeX Doc) -> HeX ()
registerEscaperFor format escaper =
  updateState $ \st -> st{ hexEscapers = M.insert format escaper
                                         $ hexEscapers st }

oneChar :: HeX Doc
oneChar = try $ do
  c <- (try $ char '\\' >> (satisfy (not . isLetter))) <|> satisfy (/='\\')
  st <- getState
  let format = hexFormat st
  let escapers = hexEscapers st
  case M.lookup format escapers of
       Just f  -> f c
       Nothing -> fail $ "No character escaper registered for format " ++
                     show format

addParser :: HeX Doc -> HeX ()
addParser p = updateState $ \st -> st{ hexParsers = p : hexParsers st }

command :: HeX Doc
command = do
  char '\\'
  cmd <- many1 letter
  skipBlank
  st <- getState
  let commands = hexCommands st
  let format = hexFormat st
  case M.lookup (cmd, Just format) commands of
        Just p  -> p
        Nothing -> case M.lookup (cmd, Nothing) commands of
                        Just q  -> q
                        Nothing -> fail $ ('\\':cmd) ++
                                     " is not defined for " ++ show format

math :: HeX Doc
math = do
  char '$'
  display <- option False $ char '$' >> return True
  let delim = if display then try (string "$$") else count 1 (char '$')
  raw <- inMathMode $ manyTill getNext delim
  format <- getFormat
  emitMath format display $ mconcat raw

emitMath :: Format -> Bool -> Doc -> HeX Doc
emitMath format display d = do
  st <- getState
  let emitters = hexEmitMath st
  case M.lookup format emitters of
       Just emitter  -> emitter display d
       Nothing       -> fail $ "No math emitter for format " ++ show format

inMathMode :: HeX a -> HeX a
inMathMode p = do
  mathmode <- liftM hexMath getState
  updateState $ \s -> s{ hexMath = True }
  res <- p
  updateState $ \s -> s { hexMath = mathmode }
  return res

ensureMath :: HeX Doc -> HeX Doc
ensureMath p = do
  mathmode <- liftM hexMath getState
  res <- inMathMode p
  format <- getFormat
  if mathmode
     then return res
     else emitMath format False res

registerEmitMathFor :: Format -> (Bool -> Doc -> HeX Doc) -> HeX ()
registerEmitMathFor format emitter =
  updateState $ \st -> st{ hexEmitMath = M.insert format emitter
                                         $ hexEmitMath st }

defaultMain :: HeX Doc -> IO ()
defaultMain parser = do
  inp <- getContents
  args <- getArgs
  when (null args) $ error "Specify output format."
  let format = CI.mk $ head args
  L.putStrLn =<< run parser format inp

