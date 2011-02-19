{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Text.HeX.Math.LaTeX (writer) where

import Text.HeX
import Text.HeX.Standard.TeX (ch)
import Control.Monad (liftM)

writer :: MathWriter
writer = MathWriter{
   startDisplayMath = "$$"
 , endDisplayMath = "$$"
 , startInlineMath = "$"
 , endInlineMath = "$"
 , grouped = \d -> "{" +++ d +++ "}"
 , variable = rawc
 , number = raws
 , operator = raws
 }
