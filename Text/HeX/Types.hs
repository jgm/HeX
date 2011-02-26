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
import Data.Maybe (fromMaybe)
import Data.CaseInsensitive (CI)
import Text.Parsec.String

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

data Mode = Block | Inline | Verbatim | Math
          deriving (Show, Eq, Ord)

newtype InlineDoc = InlineDoc Doc

newtype BlockDoc = BlockDoc Doc

data HeXState = HeXState { hexParsers   :: M.Map Mode [HeX Doc]
                         , hexMode      :: Mode
                         , hexCommands  :: M.Map (Mode, String) (HeX Doc)
                         , hexFormat    :: Format
                         , hexVars      :: M.Map String Dynamic
                         , hexTarget    :: String
                         , hexLabels    :: M.Map String String }

type HeX = ParsecT String HeXState IO

class ToCommand a where
  toCommand   :: a -> HeX Doc

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

instance ToCommand b => ToCommand (Maybe Doc -> b) where
  toCommand x = try (do char '['
                        arg <- liftM mconcat (manyTill getNext (char ']'))
                        toCommand $ x $ Just arg)
             <|> toCommand (x Nothing)

instance ToCommand b => ToCommand (Maybe String -> b) where
  toCommand x = withOpt x <|> toCommand (x Nothing)

instance ToCommand b => ToCommand (Maybe Int -> b) where
  toCommand x = withOpt x <|> toCommand (x Nothing)

instance ToCommand b => ToCommand (Maybe Integer -> b) where
  toCommand x = withOpt x <|> toCommand (x Nothing)

instance ToCommand b => ToCommand (Maybe Double -> b) where
  toCommand x = withOpt x <|> toCommand (x Nothing)

instance ToCommand b => ToCommand (OptionList -> b) where
  toCommand x = withOpt (x . fromMaybe (OptionList []))
             <|> toCommand (x (OptionList []))

instance ToCommand b => ToCommand (Doc -> b) where
  toCommand x = do arg <- group
                   toCommand (x arg)

instance ToCommand b => ToCommand (InlineDoc -> b) where
  toCommand x = do arg <- withMode Inline group
                   toCommand (x $ InlineDoc arg)

instance ToCommand b => ToCommand (BlockDoc -> b) where
  toCommand x = do arg <- withMode Block group
                   toCommand (x $ BlockDoc arg)

instance ToCommand b => ToCommand (String -> b) where
  toCommand x = do char '{'
                   arg <- manyTill anyChar (char '}')
                   toCommand $ x arg

instance ToCommand b => ToCommand (Format -> b) where
  toCommand x = do format <- liftM hexFormat getState
                   toCommand (x format)

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

group :: HeX Doc
group = try $ do
  char '{'
  res <- manyTill getNext (char '}')
  return $ mconcat res

newtype OptionList = OptionList [(String,String)]

parseOptionList :: Parser OptionList
parseOptionList  = liftM OptionList $ sepBy parseOption comma
  where comma = try $ spaces >> char ',' >> spaces

parseOption :: Parser (String,String)
parseOption = try $ do
  spaces
  key <- many1 (satisfy (/='='))
  char '='
  val <- many1 (satisfy (/=','))
  return (key,val)

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

instance ReadString OptionList where
  readString s = case parse parseOptionList "" s of
                      Left _   -> fail $ "Failed to parse `" ++ s ++ "'"
                      Right o  -> return o

getNext :: HeX Doc
getNext = do
  st <- getState
  let mode = hexMode st
  let parsers = hexParsers st
  case M.lookup mode parsers of
       Just ps -> choice ps
       Nothing -> fail $ "No parsers defined for mode " ++ show mode

readM :: (Read a, Monad m) => String -> m a
readM s | [x] <- parsed = return x
        | otherwise     = fail $ "Failed to parse `" ++ s ++ "'"
  where
    parsed = [x | (x,_) <- reads s]

raws :: String -> Doc
raws = Doc . BU.fromString

rawc :: Char -> Doc
rawc = Doc . fromChar

withMode :: Mode -> HeX a -> HeX a
withMode mode p = do
  oldmode <- liftM hexMode getState
  updateState $ \st -> st{ hexMode = mode }
  res <- p
  updateState $ \st -> st{ hexMode = oldmode }
  return res

infixl 8 +++
(+++) :: Monoid a => a -> a -> a
(+++) = mappend

data MathWriter = MathWriter{
       mathFormat       :: Format
     , displayMath      :: Doc -> Doc
     , inlineMath       :: Doc -> Doc
     , grouped          :: Doc -> Doc
     , number           :: String -> Doc
     , variable         :: Char -> Doc
     , operator         :: String -> Doc
     , whitespace       :: String -> Doc
     }
