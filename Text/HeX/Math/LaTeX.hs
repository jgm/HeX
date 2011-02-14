{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Text.HeX.Math.LaTeX (writer, commands) where

import Text.HeX
import Text.HeX.Math.Generic (math, MathWriter(..))

commands :: HeX ()
commands = do
  addParser (math writer)

writer :: MathWriter
writer = MathWriter{
   displayMath = display
 , inlineMath  = inline
 , grouped = \d -> "{" +++ d +++ "}"
 }

display :: Doc -> Doc
display b = "$$" +++ b +++ "$$"

inline :: Doc -> Doc
inline b = "$" +++ b +++ "$"
