{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Text.HeX.Math.LaTeX (commands) where

import Text.HeX
import Text.HeX.Math.Generic (math)

commands :: HeX ()
commands = do
  addParser (math parseToken emitMath)

parseToken :: HeX Doc
parseToken = oneChar -- TODO

emitMath :: Bool -> Doc -> HeX Doc
emitMath display b = do
  let delim = if display then "$$" else "$"
  return $ raws delim +++ b +++ raws delim

