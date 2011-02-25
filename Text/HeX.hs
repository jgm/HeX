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
                , register
                , addParser
                , command
                , parseDoc
                , getFormat
                , setTarget
                , addLabel
                , lookupLabel
                , warn
                , defaultMain
                , group
                , oneChar
                , comment
                , basicParsers
                , forFormat
                )
where
import Text.HeX.Types
import Text.Parsec
import qualified Data.ByteString.Lazy as L
import Blaze.ByteString.Builder.Char.Utf8 as BU
import System.Environment
import Control.Monad
import Control.Applicative ((<$>))
import Data.Dynamic
import Blaze.ByteString.Builder
import qualified Data.Map as M
import Data.Monoid
import Data.Maybe (fromMaybe)
import qualified Data.CaseInsensitive as CI
import Control.Monad.Trans (liftIO)
import System.IO
import Data.Char (isLetter)

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
  when (null s) $
    warn "Ignoring empty label."
  case M.lookup s labs of
       Just _  -> warn $ "Label " ++ show s ++ " redefined."
       Nothing -> return ()
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
               HeXState{ hexParsers = M.empty  -- was [group, command, oneChar]
                       , hexMode = Normal
                       , hexCommands = M.empty
                       , hexFormat = format
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

addParser :: Mode -> HeX Doc -> HeX ()
addParser mode p = updateState $ \st ->
  let parsers = hexParsers st in
  st{ hexParsers = if M.member mode parsers
                      then M.adjust (p:) mode parsers
                      else M.insert mode [p] parsers }

register :: ToCommand a => String -> a -> HeX ()
register name x = updateState $ \s ->
  s{ hexCommands = M.insert name (toCommand x) (hexCommands s) }

command :: HeX Doc
command = try $ do
  char '\\'
  cmd <- many1 letter <|> count 1 anyChar
  skipBlank
  st <- getState
  let commands = hexCommands st
  let format = hexFormat st
  case M.lookup cmd commands of
        Just p  -> p
        Nothing -> fail $ ('\\':cmd) ++ " is not defined for " ++ show format

warn :: String -> HeX ()
warn msg = do
  pos <- getPosition
  liftIO $ hPutStrLn stderr $
    "Warning " ++ show pos ++ ": " ++ msg

defaultMain :: HeX Doc -> IO ()
defaultMain parser = do
  inp <- getContents
  args <- getArgs
  when (null args) $ error "Specify output format."
  let format = CI.mk $ head args
  L.putStrLn =<< run parser format inp

group :: HeX Doc
group = do
  char '{'
  res <- manyTill getNext (char '}')
  return $ mconcat res

oneChar :: (Char -> Doc) -> HeX Doc
oneChar escaper = escaper <$> (try (char '\\' >> (satisfy (not . isLetter)))
                               <|> noneOf specialChars)

specialChars :: [Char]
specialChars = "%{$\\"

comment :: HeX Doc
comment = do
  char '%'
  manyTill anyChar newline
  return mempty

basicParsers :: (Char -> Doc) -> HeX Doc
basicParsers escaper = oneChar escaper <|> command <|> group <|> comment

forFormat :: Format -> HeX () -> HeX ()
forFormat f p = do
  f' <- getFormat
  when (f == f') p

