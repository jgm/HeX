module Text.HeX.Math (defaultsFor, withText) where
import Text.HeX
import Control.Monad
import Control.Applicative ((<$>))
import Data.Char (isAscii, isAlphaNum)

defaultsFor :: MathWriter -> HeX ()
defaultsFor writer = do
  addParser Math $ mathParser writer
  addParser Normal $ dollars writer
  register "(" $ parenMath writer
  register "[" $ bracketMath writer
  register "ensuremath" $ ensureMath writer

dollars :: MathWriter -> HeX Doc
dollars writer = do
  char '$'
  display <- option False $ char '$' >> return True
  spaces
  let delim = if display
                 then char '$' >> char '$'
                 else char '$'
  parseMath writer display delim

parenMath :: MathWriter -> HeX Doc
parenMath writer = spaces >> parseMath writer False (try $ string "\\)")

bracketMath :: MathWriter -> HeX Doc
bracketMath writer = spaces >> parseMath writer True (try $ string "\\]")

parseMath :: MathWriter -> Bool -> HeX a -> HeX Doc
parseMath writer display closer = do
  updateState $ \st -> st{ hexMode = Math }
  res <- liftM mconcat $ manyTill getNext closer
  updateState $ \st -> st{ hexMode = Normal }
  return $ if display
              then displayMath writer res
              else inlineMath writer res

mathParser :: MathWriter -> HeX Doc
mathParser writer = do
  res <-  command
      <|> comment
      <|> liftM (grouped writer) group
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

withText :: HeX Doc
withText = do
  current <- liftM hexMode getState
  updateState $ \st -> st{ hexMode = Normal }
  res <- getNext
  updateState $ \st -> st{ hexMode = current }
  return res

withMath :: HeX Doc
withMath = do
  current <- liftM hexMode getState
  updateState $ \st -> st{ hexMode = Math }
  res <- getNext
  updateState $ \st -> st{ hexMode = current }
  return res

ensureMath :: MathWriter -> HeX Doc
ensureMath writer = do
  current <- liftM hexMode getState
  if current == Math
     then getNext
     else inlineMath writer <$> withMath
