{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Text.HeX.Math.LaTeX (writer, commands) where

import Text.HeX
import Text.HeX.Standard.TeX (ch)
import Text.HeX.Math.Generic (math, MathWriter(..))
import Control.Monad (liftM)

commands :: HeX ()
commands = do
  registerEscaperFor "latexmath" (return . ch)
  addParser (math writer)

writer :: MathWriter
writer = MathWriter{
   displayMath = mathenv True
 , inlineMath  = mathenv False
 , grouped = \d -> "{" +++ d +++ "}"
 }

mathenv :: Bool -> HeX Doc -> HeX Doc
mathenv display p = do
  oldformat <- liftM hexFormat getState
  updateState $ \st -> st{ hexFormat = "latexmath" }
  res <- p
  updateState $ \st -> st{ hexFormat = oldformat }
  return $ if display
              then "$$" +++ res +++ "$$"
              else "$"  +++ res +++ "$"

