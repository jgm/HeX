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

module Text.HeX ( HeX
                , HeXState(..)
                , Format
                , Doc(..)
                , run
                , setVar
                , getVar
                , updateVar
                , getNext
                , cat
                , renderBS
                , (+++)
                , raws
                , rawc
                , (&)
                , (==>)
                , module Text.Parsec
                , module Data.Monoid
                )
where
import Text.Parsec
import Control.Monad
import Data.Dynamic
import qualified Data.ByteString.Lazy as L
import Text.Blaze.Builder.Core
import Text.Blaze.Builder.Utf8 as BU
import qualified Data.Map as M
import Data.Monoid
import Data.String

newtype Doc = Doc { unDoc :: Builder }
            deriving (Monoid, Typeable)

instance IsString Doc
  where fromString = Doc . BU.fromString

type Format = String

data HeXState = HeXState { hexParsers :: [HeX Doc]
                         , hexFormat  :: Format
                         , hexMath    :: Bool
                         , hexVars    :: M.Map String Dynamic }
              deriving (Typeable)

type HeX = ParsecT String HeXState IO

instance Typeable1 HeX
  where typeOf1 _ = mkTyConApp (mkTyCon "Text.HeX") []

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

setParsers :: [HeX Doc] -> HeX ()
setParsers parsers = updateState $ \s -> s{ hexParsers = parsers }

run :: [HeX Doc] -> String -> String -> IO L.ByteString
run parsers format contents = do
  result <- runParserT (do setParsers parsers
                           spaces
                           manyTill (choice parsers <|>
                                     fail "No matching parser.")  eof)
               HeXState{ hexParsers = []
                       , hexFormat = format
                       , hexMath = False
                       , hexVars = M.empty } "input" contents
  case result of
       Left e    -> error (show e)
       Right res -> return $ renderBS $ cat $ res

cat :: [Doc] -> Doc
cat = Doc . mconcat . map unDoc

raws :: String -> Doc
raws = Doc . BU.fromString

rawc :: Char -> Doc
rawc = Doc . fromChar

renderBS :: Doc -> L.ByteString
renderBS = toLazyByteString . unDoc

infixl 8 +++
(+++) :: Doc -> Doc -> Doc
Doc x +++ Doc y = Doc $ mappend x y

infixl 4 &
(&) :: HeX Doc -> HeX Doc -> HeX Doc
(&) = (<|>)

infixr 7 ==>
(==>) :: Format -> Doc -> HeX Doc
k ==> v = do
  format <- liftM hexFormat getState
  if format == k
     then return v
     else fail $ "I don't know how to render this in " ++ format

getNext :: HeX Doc
getNext = do
  parsers <- liftM hexParsers getState
  choice parsers
