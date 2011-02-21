{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Text.HeX.Math.LaTeX (defaults) where

import Text.HeX
import Text.HeX.Standard.TeX
import Control.Applicative ((<$>))
import Text.HeX.Math (defaultsFor, withText)

defaults :: HeX ()
defaults = do
  defaultsFor writer
  register "textrm" $ inCtl "textrm" <$> withText
  register "text" $ inCtl "text" <$> withText
  register "mathrm" $ inCtl "mathrm" <$> group
  register "mbox" $ inCtl "mbox" <$> group
  register "mathit" $ inCtl "mathit" <$> group
  register "textit" $ inCtl "textit" <$> withText
  register "mathtt" $ inCtl "mathtt" <$> group
  register "texttt" $ inCtl "texttt" <$> withText
  register "mathsf" $ inCtl "mathsf" <$> group
  register "mathbb" $ inCtl "mathbb" <$> group
  register "mathcal" $ inCtl "mathcal" <$> group
  register "mathfrak" $ inCtl "mathfrak" <$> group

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
