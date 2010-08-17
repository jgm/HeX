{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
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

module Text.HeX ( HeX
                , use
                , run
                , setVar
                , getVar
                , updateVar
                , (&)
                , (==>)
                , module Text.Parsec
                , module Text.Blaze.Builder.Utf8
                )
where
import Text.Parsec
import Control.Monad
import Data.Dynamic
import qualified Data.ByteString.Lazy as L
import Text.Blaze.Builder.Core
import Text.Blaze.Builder.Utf8
import qualified Data.Map as M
import Data.Monoid
import System.IO
import System.Environment
import System.Exit (ExitCode(..), exitWith)
import System.FilePath
import System.Directory

data HeXState = HeXState { hexParsers :: [HeX Builder]
                         , hexFormat  :: String
                         , hexVars    :: M.Map String Dynamic }

type HeX = ParsecT String HeXState IO

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

setParsers :: [HeX Builder] -> HeX ()
setParsers parsers = updateState $ \s -> s{ hexParsers = parsers }

run :: [HeX Builder] -> String -> String -> IO L.ByteString
run parsers format contents = do
  result <- runParserT (do setParsers parsers
                           spaces
                           manyTill (choice parsers <|>
                                     fail "No matching parser.")  eof)
               HeXState{ hexParsers = []
                       , hexFormat = format
                       , hexVars = M.empty } "input" contents
  case result of
       Left e    -> error (show e)
       Right res -> return $ toLazyByteString $ mconcat $ res

usage :: IO ()
usage = do
  prog' <- getInputFilePath
  hPutStrLn stderr $ "HeX (c) 2010 John MacFarlane\n" ++
                     "Usage:  ./" ++ prog' ++ " FORMAT"

use :: [HeX Builder] -> IO ()
use parsers = do
  prog' <- getInputFilePath
  args <- getArgs
  format <- case args of
             [x]    -> return x
             _      -> usage >> exitWith (ExitFailure 1)
  txt <- liftM removeCode $ readFile prog'
  res <- run parsers format txt
  L.putStr res
  exitWith ExitSuccess

removeCode :: String -> String
removeCode = unlines . map (\ln -> if isCommentLine ln then ln else "") . lines
   where isCommentLine :: String -> Bool
         isCommentLine ('>':_) = False
         isCommentLine ('#':_) = False
         isCommentLine _       = True

getInputFilePath :: IO FilePath
getInputFilePath = do
  prog' <- getProgName >>= makeRelativeToCurrentDirectory
  case takeExtension prog' of
        ".lhs" -> return prog'
        _      -> error $ "`" ++ prog' ++ "' is not a literate Haskell file."

infixl 4 &
(&) :: HeX Builder -> HeX Builder -> HeX Builder
(&) = (<|>)

infixr 7 ==>
(==>) :: String -> Builder -> HeX Builder
k ==> v = do
  format <- liftM hexFormat getState
  if format == k
     then return v
     else fail $ "I don't know how to render this in " ++ format

