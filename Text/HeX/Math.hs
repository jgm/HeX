module Text.HeX.Math (defaultsFor, arrayLines) where
import Text.HeX
import Control.Applicative ((<$>))
import Data.Char (isAscii, isAlphaNum)

defaultsFor :: MathWriter -> HeX ()
defaultsFor writer = do
  addParser [Math] $ mathParser writer
  addParser [Block,Inline] $ dollars writer
  newCommand [Inline] "(" $ parenMath writer
  newCommand [Inline,Block] "[" $ bracketMath writer
  newCommand [Inline] "ensuremath" $ ensureMath Inline writer
  newCommand [Math] "ensuremath" $ ensureMath Math writer

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
  setVar "displaymath" display
  res <- mconcat <$> manyTill math closer
  setVar "displaymath" False
  return $ if display
              then displayMath writer res
              else inlineMath writer res

mathParser :: MathWriter -> HeX Doc
mathParser writer = do
  spaces
  res <-  environment Math
      <|> command Math
      <|> comment
      <|> grouped writer <$> group math
      <|> number writer <$> pNumber
      <|> variable writer <$> pVariable
      <|> operator writer <$> pOperator
      <|> (operator writer . (:[])) <$> (pEscaped <|> pUnicode)
  spaces
  return res

opLetters :: [Char]
opLetters = ":+*/=-(),;.?'~[]<>!"

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

ensureMath :: Mode -> MathWriter -> HeX Doc
ensureMath current writer = do
  if current == Math
     then math
     else inlineMath writer <$> math

endLine :: HeX ()
endLine = try $ do
  string "\\\\"
  optional inbrackets  -- can contain e.g. [1.0in] for a line height, not yet supported
  return ()

inbrackets :: HeX String
inbrackets = try $ char '[' >> manyTill (satisfy (/=']')) (char ']')

arrayLine :: HeX Doc -> HeX [Doc]
arrayLine expr =
  sepBy1 (mconcat <$> many (notFollowedBy endLine >> expr)) (char '&')

arrayLines :: HeX Doc -> HeX [[Doc]]
arrayLines expr = sepEndBy1 (arrayLine expr) endLine
