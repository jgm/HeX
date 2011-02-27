module Text.HeX.Math (defaultsFor) where
import Text.HeX
import Control.Applicative ((<$>))
import Data.Char (isAscii, isAlphaNum)

defaultsFor :: MathWriter -> HeX ()
defaultsFor writer = do
  addParser [Math] $ mathParser writer
  addParser [Block,Inline] $ dollars writer
  register [Inline] "(" $ parenMath writer
  register [Inline,Block] "[" $ bracketMath writer
  register [Inline] "ensuremath" $ ensureMath Inline writer
  register [Math] "ensuremath" $ ensureMath Math writer

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
  res <- mconcat <$> manyTill math closer
  return $ if display
              then displayMath writer res
              else inlineMath writer res

mathParser :: MathWriter -> HeX Doc
mathParser writer = do
  res <-  command Math
      <|> comment
      <|> grouped writer <$> group math
      <|> number writer <$> pNumber
      <|> variable writer <$> pVariable
      <|> operator writer <$> pOperator
      <|> (operator writer . (:[])) <$> (pEscaped <|> pUnicode)
      <|> whitespace writer <$> many1 space
  -- NOTE - may need to put enclosures in here too.
  -- OR, perhaps better: put this in MathML module,
  -- and add a flag in state for when we're checking for subscript?
  -- limits <- limitsIndicator
  -- subSup limits a <|> superOrSubscripted limits a <|> return a
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

ensureMath :: Mode -> MathWriter -> HeX Doc
ensureMath current writer = do
  if current == Math
     then math
     else inlineMath writer <$> math

{-

expr :: GenParser Char st Exp
expr = do
  a <- expr1
  limits <- limitsIndicator
  subSup limits a <|> superOrSubscripted limits a <|> return a

limitsIndicator :: GenParser Char st (Maybe Bool)
limitsIndicator =
   try (symbol "\\limits" >> return (Just True))
  <|> try (symbol "\\nolimits" >> return (Just False))
  <|> return Nothing

subSup :: Maybe Bool -> Exp -> GenParser Char st Exp
subSup limits a = try $ do
  char '_'
  b <- expr1
  char '^'
  c <- expr
  return $ case limits of
            Just True  -> EUnderover a b c
            Nothing | isConvertible a -> EDownup a b c
            _          -> ESubsup a b c

superOrSubscripted :: Maybe Bool -> Exp -> GenParser Char st Exp
superOrSubscripted limits a = try $ do
  c <- oneOf "^_"
  b <- expr
  case c of
       '^' -> return $ case limits of
                        Just True  -> EOver a b
                        Nothing | isConvertible a -> EUp a b
                        _          -> ESuper a b
       '_' -> return $ case limits of
                        Just True  -> EUnder a b
                        Nothing | isConvertible a -> EDown a b
                        _          -> ESub a b
       _   -> pzero

isConvertible :: Exp -> Bool
isConvertible (EMathOperator x) = x `elem` convertibleOps
  where convertibleOps = ["lim","liminf","limsup","inf","sup"]
isConvertible (ESymbol Rel _) = True
isConvertible (ESymbol Bin _) = True
isConvertible (EUnder _ _)    = True
isConvertible (EOver _ _)     = True
isConvertible (EUnderover _ _ _) = True
isConvertible (ESymbol Op x) = x `elem` convertibleSyms
  where convertibleSyms = ["\x2211","\x220F","\x22C2",
           "\x22C3","\x22C0","\x22C1","\x2A05","\x2A06",
           "\x2210","\x2A01","\x2A02","\x2A00","\x2A04"]
isConvertible _ = False

-}


