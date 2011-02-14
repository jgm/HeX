-- based on texmath

module Text.HeX.Math.Generic
where

import Control.Monad
import Data.Char (isAlphaNum, isDigit, isAscii)
import qualified Data.Map as M
import Text.HeX

data MathWriter = MathWriter{
       displayMath :: Doc -> Doc
     , inlineMath  :: Doc -> Doc
     , grouped     :: Doc -> Doc
     }

math :: MathWriter -> HeX Doc
math writer = do
  char '$'
  display <- option False $ char '$' >> return True
  let delim = if display then try (string "$$") else count 1 (char '$')
  raw <- inMathMode $ manyTill (parseToken writer) delim
  return $ if display
              then displayMath writer $ mconcat raw
              else inlineMath writer  $ mconcat raw

parseToken :: MathWriter -> HeX Doc
parseToken writer =  liftM (grouped writer) group
                 <|> oneChar -- FOR NOW

inMathMode :: HeX a -> HeX a
inMathMode p = do
  mathmode <- liftM hexMath getState
  updateState $ \s -> s{ hexMath = True }
  res <- p
  updateState $ \s -> s { hexMath = mathmode }
  return res

ensureMath :: (Bool -> Doc -> HeX Doc) -> HeX Doc -> HeX Doc
ensureMath emitter p = do
  mathmode <- liftM hexMath getState
  res <- inMathMode p
  if mathmode
     then return res
     else emitter False res

