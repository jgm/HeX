{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Text.HeX.Math.LaTeX (math) where

import Text.HeX
import Text.HeX.Standard.TeX (ch)
import Control.Monad (liftM)

math :: Format -> HeX ()
math f = do
  registerMathWriterFor f writer

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
