{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, PatternGuards, StandaloneDeriving, FlexibleInstances, TypeSynonymInstances #-}
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
                , run
                , setVar
                , getVar
                , updateVar
                , getNext
                , (+++)
                , (&)
                , (==>)
                , module Text.Parsec
                , module Text.Blaze.Builder.Utf8
                , module Data.Monoid
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
import Data.String

data HeXState = HeXState { hexParsers :: [HeX Builder]
                         , hexFormat  :: String
                         , hexMath    :: Bool
                         , hexVars    :: M.Map String Dynamic }
              deriving (Typeable)

type HeX = ParsecT String HeXState IO

deriving instance Typeable Builder

instance Typeable1 HeX
  where typeOf1 _ = mkTyConApp (mkTyCon "HeX") []

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
                       , hexMath = False
                       , hexVars = M.empty } "input" contents
  case result of
       Left e    -> error (show e)
       Right res -> return $ toLazyByteString $ mconcat $ res

infixl 8 +++
(+++) :: Builder -> Builder -> Builder
(+++) = mappend

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

instance IsString Builder
  where fromString = Text.Blaze.Builder.Utf8.fromString

getNext :: HeX Builder
getNext = do
  parsers <- liftM hexParsers getState
  choice parsers
