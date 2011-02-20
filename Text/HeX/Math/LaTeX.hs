{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Text.HeX.Math.LaTeX (mathFor) where

import Text.HeX
import Text.HeX.Standard.TeX
import Control.Applicative ((<$>))
import Text.HeX.Math (withText)
import qualified Text.HeX.Standard.Xml as Xml

mathFor :: Format -> HeX ()
mathFor f = do
  registerMathWriterFor f writer
  registerEscaperFor "mathml" (return . Xml.ch)
  registerFor "latex" "textrm" $ inCtl "textrm" <$> withText
  registerFor "latex" "text" $ inCtl "text" <$> withText
  registerFor "latex" "mathrm" $ inCtl "mathrm" <$> group
  registerFor "latex" "mbox" $ inCtl "mbox" <$> group
  registerFor "latex" "mathit" $ inCtl "mathit" <$> group
  registerFor "latex" "textit" $ inCtl "textit" <$> withText
  registerFor "latex" "mathtt" $ inCtl "mathtt" <$> group
  registerFor "latex" "texttt" $ inCtl "texttt" <$> withText
  registerFor "latex" "mathsf" $ inCtl "mathsf" <$> group
  registerFor "latex" "mathbb" $ inCtl "mathbb" <$> group
  registerFor "latex" "mathcal" $ inCtl "mathcal" <$> group
  registerFor "latex" "mathfrak" $ inCtl "mathfrak" <$> group

inCtl :: String -> Doc -> Doc
inCtl s d = ctl s +++ grp [d]

writer :: MathWriter
writer = MathWriter{
   mathFormat = "latex"
 , displayMath = \d ->
     "$$" +++ d +++ "$$"
 , inlineMath = \d ->
     "$" +++ d +++ "$"
 , grouped = \d -> "{" +++ d +++ "}"
 , variable = rawc
 , number = raws
 , operator = raws
 }
