-- based on texmath

module Text.HeX.Math.Generic
where

import Control.Monad
import Data.Char (isAlphaNum, isDigit, isAscii)
import qualified Data.Map as M
import Text.HeX

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

data MathWriter = MathWriter{
       displayMath :: HeX Doc -> HeX Doc
     , inlineMath  :: HeX Doc -> HeX Doc
     , grouped     :: Doc -> Doc
     }


math :: MathWriter -> HeX Doc
math writer = do
  char '$'
  display <- option False $ char '$' >> return True
  let delim = if display
                 then try (string "$$") >> return ()
                 else char '$' >> return ()
  let env = if display
               then displayMath writer
               else inlineMath writer
  env $ liftM mconcat $ manyTill (parseToken writer) delim

parseToken :: MathWriter -> HeX Doc
parseToken writer =  liftM (grouped writer) group
                 <|> oneChar -- FOR NOW

