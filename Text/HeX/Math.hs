module Text.HeX.Math where
import Text.HeX.Types
import Text.Parsec
import Control.Monad
import Data.Monoid
import qualified Data.Map as M
import Data.Char (isLetter)

registerMathWriterFor :: Format -> MathWriter -> HeX ()
registerMathWriterFor format writer =
  updateState $ \st -> st{ hexMathWriters = M.insert format writer
                                           $ hexMathWriters st }

getMathWriter :: HeX MathWriter
getMathWriter = do
  st <- getState
  let format = hexFormat st
  let mathwriters = hexMathWriters st
  case M.lookup format mathwriters of
       Just w   -> return w
       Nothing  -> fail $ "No math writer defined for format " ++ show format

math :: HeX Doc
math = do
  char '$'
  display <- option False $ char '$' >> return True
  spaces
  writer <- getMathWriter
  let delim = if display
                 then try (string "$$") >> return ()
                 else char '$' >> return ()
  let env = if display
               then displayMath writer
               else inlineMath writer
  parsers <- liftM hexParsers getState
  updateState $ \st -> st{ hexParsers = mathParser writer : parsers }
  res <- env $ liftM mconcat $ manyTill getNext delim
  updateState $ \st -> st{ hexParsers = parsers }
  return res

mathParser :: MathWriter -> HeX Doc
mathParser writer = do
  res <-  liftM (grouped writer) group
      <|> liftM (number writer) pNumber
      <|> liftM (variable writer) pVariable
  spaces
  return res

opLetters :: [Char]
opLetters = ":_+*/=^-(),;.?'~[]<>!"

pOperator :: HeX String
pOperator = count 1 (oneOf opLetters) <|> many1 (char '\'')

pNumber :: HeX String
pNumber = many1 digit

pVariable :: HeX Char
pVariable = letter
