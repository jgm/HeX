{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Text.HeX.Math.LaTeX (writer) where

import Text.HeX
import Text.HeX.Standard.TeX (ch)
import Control.Monad (liftM)

writer :: MathWriter
writer = MathWriter{
   displayMath = mathenv True
 , inlineMath  = mathenv False
 , grouped = \d -> "{" +++ d +++ "}"
 , variable = id
 , number = id
 }

mathenv :: Bool -> HeX Doc -> HeX Doc
mathenv display p = do
  res <- p
  return $ if display
              then "$$" +++ res +++ "$$"
              else "$"  +++ res +++ "$"

