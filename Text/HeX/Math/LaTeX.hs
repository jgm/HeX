{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Text.HeX.Math.LaTeX (mathFor) where

import Text.HeX
import Text.HeX.Standard.TeX
import Control.Applicative ((<$>))
import Text.HeX.Math (withText)

mathFor :: Format -> HeX ()
mathFor f = do
  registerMathWriterFor f writer
  registerFor f "textrm" $ inCtl "textrm" <$> withText
  registerFor f "text" $ inCtl "text" <$> withText
  registerFor f "mathrm" $ inCtl "mathrm" <$> group
  registerFor f "mbox" $ inCtl "mbox" <$> group
  registerFor f "mathit" $ inCtl "mathit" <$> group
  registerFor f "textit" $ inCtl "textit" <$> withText
  registerFor f "mathtt" $ inCtl "mathtt" <$> group
  registerFor f "texttt" $ inCtl "texttt" <$> withText
  registerFor f "mathsf" $ inCtl "mathsf" <$> group
  registerFor f "mathbb" $ inCtl "mathbb" <$> group
  registerFor f "mathcal" $ inCtl "mathcal" <$> group
  registerFor f "mathfrak" $ inCtl "mathfrak" <$> group

inCtl :: String -> Doc -> Doc
inCtl s d = ctl s +++ grp [d]

writer :: MathWriter
writer = MathWriter{
   displayMath = \d ->
     "$$" +++ d +++ "$$"
 , inlineMath = \d ->
     "$" +++ d +++ "$"
 , grouped = \d -> "{" +++ d +++ "}"
 , variable = rawc
 , number = raws
 , operator = raws
 }
