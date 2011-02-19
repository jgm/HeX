module Text.HeX.Math where
import Text.HeX.Types
import Text.Parsec
import Control.Monad
import Data.Monoid
import qualified Data.Map as M
import Data.Char (isAscii, isAlphaNum)

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

dollars :: HeX Doc
dollars = do
  char '$'
  display <- option False $ char '$' >> return True
  spaces
  let delim = if display
                 then try (string "$$") >> return ()
                 else char '$' >> return ()
  openMath display delim

openMath :: Bool -> HeX a -> HeX Doc
openMath display closer = do
  writer <- getMathWriter
  parsers <- liftM hexParsers getState
  let endMath = do closer
                   updateState $ \st -> st{ hexParsers = parsers }
                   return $ if display
                               then endDisplayMath writer
                               else endInlineMath writer
  updateState $ \st -> st{ hexParsers = endMath : mathParser writer : parsers }
  return $ if display
              then startDisplayMath writer
              else startInlineMath writer

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


