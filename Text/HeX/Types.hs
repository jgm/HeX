{-# LANGUAGE FlexibleInstances, TypeSynonymInstances,
    GeneralizedNewtypeDeriving, PatternGuards #-}
module Text.HeX.Types
where
import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8 as BU
import Data.ByteString.Lazy.UTF8 (toString)
import qualified Data.Map as M
import Data.Monoid
import Data.String
import Text.Parsec
import Control.Monad
import Data.Dynamic
import Data.CaseInsensitive (CI)

data Doc = Doc Builder
         | Fut (HeXState -> HeX Builder)

instance Monoid Doc where
  mempty = Doc mempty
  mappend (Doc x) (Doc y) = Doc (mappend x y)
  mappend (Doc x) (Fut f) = Fut $ \st -> liftM (x `mappend`) $ f st
  mappend (Fut f) (Doc x) = Fut $ \st -> liftM (`mappend` x) $ f st
  mappend (Fut f) (Fut g) = Fut $ \st -> liftM2 mappend (f st) (g st)

instance IsString Doc
  where fromString = Doc . BU.fromString

type Format = CI String

data HeXState = HeXState { hexParsers     :: [HeX Doc]
                         , hexEscapers    :: M.Map Format (Char -> HeX Doc)
                         , hexCommands    :: M.Map (String, (Maybe Format))
                                              (HeX Doc)
                         , hexMathWriters :: M.Map Format MathWriter
                         , hexFormat      :: Format
                         , hexVars        :: M.Map String Dynamic
                         , hexTarget      :: String
                         , hexLabels      :: M.Map String String }

type HeX = ParsecT String HeXState IO

class ToCommand a where
  toCommand   :: a -> HeX Doc
  register    :: String -> a -> HeX ()
  registerFor :: Format -> String -> a -> HeX ()

  register name x = updateState $ \s ->
    s{ hexCommands = M.insert (name, Nothing)
       (toCommand x) (hexCommands s) }

  registerFor f name x = updateState $ \s ->
    s{ hexCommands = M.insert (name, Just f)
       (toCommand x) (hexCommands s) }

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

instance ToCommand b => ToCommand (Maybe String -> b) where
  toCommand x = withOpt x <|> toCommand (x Nothing)

instance ToCommand b => ToCommand (Maybe Int -> b) where
  toCommand x = withOpt x <|> toCommand (x Nothing)

instance ToCommand b => ToCommand (Maybe Integer -> b) where
  toCommand x = withOpt x <|> toCommand (x Nothing)

instance ToCommand b => ToCommand (Maybe Double -> b) where
  toCommand x = withOpt x <|> toCommand (x Nothing)

instance ToCommand b => ToCommand (Doc -> b) where
  toCommand x = do arg <- group
                   toCommand (x arg)

instance ToCommand b => ToCommand (String -> b) where
  toCommand x = group >>= withArg x

instance ToCommand b => ToCommand (Int -> b) where
  toCommand x = group >>= withArg x

instance ToCommand b => ToCommand (Integer -> b) where
  toCommand x = group >>= withArg x

instance ToCommand b => ToCommand (Double -> b) where
  toCommand x = group >>= withArg x

instance ToCommand b => ToCommand ([Doc] -> b) where
  toCommand x = do arg <- sepBy group spaces
                   toCommand (x arg)

withOpt :: (ToCommand b, ReadString a)
        => (Maybe a -> b) -> HeX Doc
withOpt f = try $ do char '['
                     arg <- liftM mconcat $ manyTill getNext (char ']')
                     withArg (f . Just) arg

withArg :: (ToCommand b, ReadString a)
        => (a -> b) -> Doc -> HeX Doc
withArg x arg = do
     let bToS = toString . toLazyByteString
     let bToA d = case readString (bToS d) of
                       Just r  -> return r
                       Nothing -> unexpected (bToS d)
     case arg of
          (Doc d) -> bToA d >>= toCommand . x
          (Fut f) -> return $ Fut $ \st -> do
                       d <- f st
                       res <- bToA d >>= toCommand . x
                       case res of
                            (Doc b) -> return b
                            (Fut _) -> error "Unexpected Fut"

class ReadString a where
  readString :: String -> Maybe a

instance ReadString String where
  readString = Just

instance ReadString Int where
  readString = readM

instance ReadString Integer where
  readString = readM

instance ReadString Double where
  readString = readM

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

raws :: String -> Doc
raws = Doc . BU.fromString

rawc :: Char -> Doc
rawc = Doc . fromChar

infixl 8 +++
(+++) :: Monoid a => a -> a -> a
(+++) = mappend

data MathWriter = MathWriter{
       displayMath :: HeX Doc -> HeX Doc
     , inlineMath  :: HeX Doc -> HeX Doc
     , grouped     :: Doc -> Doc
     }
