module Text.HeX.Math (math) where
import Text.HeX.Types
import Text.Parsec
import Control.Monad
import Data.Monoid
import qualified Data.Map as M
import Data.Char (isAscii, isAlphaNum)

getMathWriter :: HeX MathWriter
getMathWriter = do
  st <- getState
  let format = hexFormat st
  let mathwriters = hexMathWriters st
  case M.lookup format mathwriters of
       Just w   -> return w
       Nothing  -> fail $ "No math writer defined for format " ++ show format

math :: HeX Doc
math = dollars <|> parenMath <|> bracketMath

dollars :: HeX Doc
dollars = do
  char '$'
  display <- option False $ char '$' >> return True
  spaces
  let delim = if display
                 then char '$' >> char '$'
                 else char '$'
  parseMath display delim

parenMath :: HeX Doc
parenMath = do
  try $ string "\\("
  spaces
  parseMath False (try $ string "\\)")

bracketMath :: HeX Doc
bracketMath = do
  try $ string "\\["
  spaces
  parseMath True (try $ string "\\]")

parseMath :: Bool -> HeX a -> HeX Doc
parseMath display closer = do
  writer <- getMathWriter
  parsers <- liftM hexParsers getState
  updateState $ \st -> st{ hexParsers = mathParser writer : parsers }
  res <- liftM mconcat $ manyTill getNext closer
  updateState $ \st -> st{ hexParsers = parsers }
  return $ if display
              then displayMath writer res
              else inlineMath writer res

mathParser :: MathWriter -> HeX Doc
mathParser writer = do
  res <-  liftM (grouped writer) group
      <|> liftM (number writer) pNumber
      <|> liftM (variable writer) pVariable
      <|> liftM (operator writer) pOperator
      <|> liftM (operator writer . (:[])) (pEscaped <|> pUnicode)
  spaces
  return res

opLetters :: [Char]
opLetters = ":_+*/=^-(),;.?'~[]<>!"

pOperator :: HeX String
pOperator = many1 (char '\'') <|> count 1 (oneOf opLetters)

pNumber :: HeX String
pNumber = many1 digit

pVariable :: HeX Char
pVariable = letter

pEscaped :: HeX Char
pEscaped = try $ char '\\' >> satisfy (not . isAlphaNum)

pUnicode :: HeX Char
pUnicode = satisfy (not . isAscii)


