{-# LANGUAGE DeriveDataTypeable, FlexibleInstances,
    TypeSynonymInstances, GeneralizedNewtypeDeriving #-}
module Text.HeX.Types
where
import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8 as BU
import qualified Data.Map as M
import Data.Monoid
import Data.String
import Text.Parsec
import qualified Data.ByteString.Lazy.UTF8 as U
import Control.Monad
import Data.Dynamic


newtype Doc = Doc { unDoc :: Builder }
            deriving (Monoid, Typeable)

instance IsString Doc
  where fromString = Doc . BU.fromString

data Format = Html | LaTeX
            deriving (Read, Show, Eq)

data HeXState = HeXState { hexParsers  :: [HeX Doc]
                         , hexCommands :: M.Map String (HeX Doc)
                         , hexFormat   :: Format
                         , hexMath     :: Bool
                         , hexVars     :: M.Map String Dynamic }
              deriving (Typeable)

type HeX = ParsecT String HeXState IO

instance Typeable1 HeX
  where typeOf1 _ = mkTyConApp (mkTyCon "Text.HeX") []

class ToCommand a where
  toCommand :: a -> HeX Doc

instance ToCommand (HeX Doc) where
  toCommand x = x

instance ToCommand a => ToCommand (Format -> a) where
  toCommand x = do format <- liftM hexFormat getState
                   toCommand (x format)

instance ToCommand b => ToCommand (Doc -> b) where
  toCommand x = do arg <- group
                   toCommand (x arg)

instance (ToCommand b) => ToCommand ([Doc] -> b) where
  toCommand x = do args <- many group
                   toCommand (x args)

instance (ToCommand b) => ToCommand (Integer -> b) where
  toCommand x = do arg <- group
                   arg' <- readM (docToStr arg)
                   toCommand (x arg')

instance (ToCommand b) => ToCommand (String -> b) where
  toCommand x = do arg <- group
                   toCommand (x $ docToStr arg)

instance (Read a, ToCommand b) => ToCommand (Maybe a -> b) where
  toCommand x = do opt <- getOpt
                   toCommand (x opt)

instance ToCommand Doc where
  toCommand x = return x

docToStr :: Doc -> String
docToStr = U.toString . toLazyByteString . unDoc

getNext :: HeX Doc
getNext = do
  parsers <- liftM hexParsers getState
  choice parsers

group :: HeX Doc
group = do
  char '{'
  res <- manyTill getNext (char '}')
  return $ cat res

readM :: (Read a, Monad m) => String -> m a
readM s | [x] <- parsed = return x
        | otherwise     = fail $ "Failed to parse `" ++ s ++ "'"
  where
    parsed = [x | (x,_) <- reads s]

getOpt :: (Monad m, Read a) => HeX (m a)
getOpt = try $ do
  char '['
  liftM readM $ manyTill anyChar (char ']')

cat :: [Doc] -> Doc
cat = Doc . mconcat . map unDoc

raws :: String -> Doc
raws = Doc . BU.fromString

rawc :: Char -> Doc
rawc = Doc . fromChar

infixl 8 +++
(+++) :: Doc -> Doc -> Doc
Doc x +++ Doc y = Doc $ mappend x y


