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

-- TODO: this needs rearranging.
-- perhaps MathWriter should go in HeXState
-- we'd need a registerMathWriter.
-- then, this module can register generic math commands.
-- when these commands go into math mode, they put a bunch
-- of new parsers into play; when out of math mode, they
-- remove them.
-- How do the new parsers come into play? If we put them
-- at the beginning of the parser list, they'll overshadow
-- macros etc.
-- Maybe we need separate lists of math-commands and non-math-commands
-- in hexState, and separate registerMathCommand etc.
-- then math writer can simply registerMathCommand for all the commands
-- appropriate for the writer.

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
  mathmode <- liftM ((== Math) . hexMode) getState
  updateState $ \s -> s{ hexMode = Math }
  res <- p
  updateState $ \s -> s { hexMode = Math }
  return res

ensureMath :: (Bool -> Doc -> HeX Doc) -> HeX Doc -> HeX Doc
ensureMath emitter p = do
  mathmode <- liftM ((== Math) . hexMode) getState
  res <- inMathMode p
  if mathmode
     then return res
     else emitter False res

