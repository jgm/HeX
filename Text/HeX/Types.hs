{-# LANGUAGE FlexibleInstances, TypeSynonymInstances,
    GeneralizedNewtypeDeriving, PatternGuards #-}
module Text.HeX.Types
where
import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8 as BU
import qualified Data.Map as M
import Data.Monoid
import Data.String
import Text.Parsec
import Control.Monad
import Data.Dynamic


data Doc = Doc Builder
         | Fut (HeXState -> Builder)

instance Monoid Doc where
  mempty = Doc mempty
  mappend (Doc x) (Doc y) = Doc (mappend x y)
  mappend (Doc x) (Fut f) = Fut (\l -> mappend x (f l))
  mappend (Fut f) (Doc x) = Fut (\l -> mappend (f l) x)
  mappend (Fut f) (Fut g) = Fut (\l -> mappend (f l) (g l))

instance IsString Doc
  where fromString = Doc . BU.fromString

data Format = Html | LaTeX
            deriving (Read, Show, Eq)

data HeXState = HeXState { hexParsers  :: [HeX Doc]
                         , hexCommands :: M.Map String (HeX Doc)
                         , hexFormat   :: Format
                         , hexMath     :: Bool
                         , hexVars     :: M.Map String Dynamic
                         , hexLabels   :: M.Map String String }

type HeX = ParsecT String HeXState IO

class ToCommand a where
  toCommand :: a -> HeX Doc

instance ToCommand Doc where
  toCommand = return

instance ToCommand [Doc] where
  toCommand = return . mconcat

instance ToCommand Integer where
  toCommand = return . raws . show

instance ToCommand Double where
  toCommand = return . raws . show

instance ToCommand (HeX Doc) where
  toCommand = id

instance ToCommand (HeX [Doc]) where
  toCommand = liftM mconcat

instance ToCommand (HeX Integer) where
  toCommand = liftM (raws . show)

instance ToCommand a => ToCommand (Format -> a) where
  toCommand x = do format <- liftM hexFormat getState
                   toCommand (x format)

instance ToCommand b => ToCommand (Doc -> b) where
  toCommand x = do arg <- group
                   toCommand (x arg)

instance (ToCommand b) => ToCommand ([Doc] -> b) where
  toCommand x = do args <- many group
                   toCommand (x args)

instance (Read a, ToCommand b) => ToCommand (Maybe a -> b) where
  toCommand x = do opt <- getOpt
                   toCommand (x opt)

instance ToCommand b => ToCommand (String -> b) where
  toCommand x = do char '{'
                   arg <- manyTill anyChar (char '}')
                   toCommand (x arg)

getNext :: HeX Doc
getNext = do
  parsers <- liftM hexParsers getState
  choice parsers

group :: HeX Doc
group = do
  char '{'
  res <- manyTill getNext (char '}')
  return $ mconcat res

readM :: (Read a, Monad m) => String -> m a
readM s | [x] <- parsed = return x
        | otherwise     = fail $ "Failed to parse `" ++ s ++ "'"
  where
    parsed = [x | (x,_) <- reads s]

getOpt :: (Monad m, Read a) => HeX (m a)
getOpt = try $ do
  char '['
  liftM readM $ manyTill anyChar (char ']')

raws :: String -> Doc
raws = Doc . BU.fromString

rawc :: Char -> Doc
rawc = Doc . fromChar

infixl 8 +++
(+++) :: Monoid a => a -> a -> a
(+++) = mappend

