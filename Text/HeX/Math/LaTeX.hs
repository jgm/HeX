{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Text.HeX.Math.LaTeX (commands, emitMath) where

import Text.HeX

commands :: HeX ()
commands = do
  registerEmitMathFor "latex" emitMath

emitMath :: Bool -> Doc -> HeX Doc
emitMath display b = do
  let delim = if display then "$$" else "$"
  return $ raws delim +++ b +++ raws delim

