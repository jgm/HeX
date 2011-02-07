-- load with hugs -98

-- try:
-- unDoc mydoc stdout Html
-- test [command "emph" emph] "\emph{hi there} buddy" Html

import qualified Data.Map as M
import Data.Dynamic (Dynamic)
import Data.Monoid
import Control.Monad
import System.IO
import Text.ParserCombinators.Parsec

data Format = Html | TeX deriving (Eq, Show, Read)

newtype Doc = Doc { unDoc :: Handle -> Format -> IO () }

instance Monoid Doc where
  mempty                      = Doc $ \_ _ -> return ()
  Doc x `mappend` Doc y = Doc $ \h f -> x h f >> y h f

(+++) :: Doc -> Doc -> Doc
(+++) = mappend

data HeXState = HeXState{ hexParsers :: [HeX Doc] }

type HeX = GenParser Char HeXState

class ToCommand a where
  toCommand :: a -> HeX Doc

instance ToCommand (HeX Doc) where
  toCommand x = x

instance ToCommand b => ToCommand (Doc -> b) where
  toCommand x = try $ do arg <- getNext
                         toCommand (x arg)

instance (Read a, ToCommand b) => ToCommand (Maybe a -> b) where
  toCommand x = do opt <- getOpt
                   toCommand (x opt)

instance ToCommand Doc where
  toCommand x = return x

getNext :: HeX Doc
getNext = do
  parsers <- liftM hexParsers getState
  choice parsers

readM :: (Read a, Monad m) => String -> m a
readM s = case [x | (x,_) <- reads s] of
                [y]   -> return y
                _     -> fail $ "Failed to parse `" ++ s ++ "'"

getOpt :: Read a => HeX (Maybe a)
getOpt = option Nothing $ try $ do
  char '['
  raw <- manyTill oneChar (char ']')
  return $ readM raw

oneChar :: HeX Char
oneChar = (char '\\' >> anyChar) <|> anyChar

ch :: Char -> Doc
ch c = Doc $ \h f ->
         case f of
              Html  -> hPutHtmlChar h c
              TeX   -> hPutTeXChar  h c

put :: Format -> Doc -> Doc
put f (Doc d) = Doc $ \h f' -> if f == f'
                                  then d h f
                                  else return ()

out :: Format -> Doc -> IO ()
out format d = (unDoc d) stdout format

lit :: String -> Doc
lit x = Doc $ \h _ -> hPutStr h x

str :: String -> Doc
str x = Doc $ \h f ->
          case f of
               Html -> mapM_ (hPutHtmlChar h) x
               TeX  -> mapM_ (hPutTeXChar h) x

hPutHtmlChar h '&' = hPutStr h "&amp;"
hPutHtmlChar h '<' = hPutStr h "&lt;"
hPutHtmlChar h '>' = hPutStr h "&gt;"
hPutHtmlChar h '"' = hPutStr h "&quot;"
hPutHtmlChar h x =   hPutChar h x

hPutTeXChar h x = hPutChar h x

tok :: HeX Doc
tok = group <|> liftM ch oneChar

emph :: Doc -> Doc
emph x = put Html (lit "<em>" +++ x +++ lit "</em>") +++
         put TeX  (lit "\\emph{" +++ x +++ lit "}")

mydoc :: Doc
mydoc = mconcat [
    str "hi"
  , emph $ str "hey"
  , str "there"
  ]

group :: HeX Doc
group = try $ do
  char '{'
  liftM mconcat $ manyTill getNext (char '}')

command :: ToCommand a => String -> a -> HeX Doc
command name cmd = try $ do
  char '\\'
  string name
  toCommand cmd

parseHeX :: [HeX Doc] -> String -> Doc
parseHeX ps s =
  case runParser (many getNext) HeXState{ hexParsers = ps ++ [tok] } "input" s of
       Right x  -> mconcat x
       Left e   -> error $ show e

test ps s = unDoc (parseHeX ps s) stdout

