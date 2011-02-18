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
  env $ liftM mconcat $ manyTill (mathParser writer) delim

mathParser :: MathWriter -> HeX Doc
mathParser writer = do
  res <- liftM (grouped writer) group
      <|> liftM (number writer) pNumber
      <|> aChar -- FOR NOW
  spaces
  return res

pNumber :: HeX Doc
pNumber = liftM raws $ many1 digit

-- TODO - this is just for now, replace w/ something better
aChar :: HeX Doc
aChar = try $ do
  c <- (try $ char '\\' >> (satisfy (not . isLetter))) <|> satisfy (/='\\')
  st <- getState
  let format = hexFormat st
  let escapers = hexEscapers st
  case M.lookup format escapers of
       Just f  -> f c
       Nothing -> fail $ "No character escaper registered for format " ++
                     show format



