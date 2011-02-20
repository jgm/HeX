{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, PackageImports #-}
module Text.HeX.Standard ( defaults ) where

import Text.HeX
import qualified Text.HeX.Standard.Generic as Generic
import qualified Text.HeX.Standard.Html as Html
import qualified Text.HeX.Standard.LaTeX as LaTeX
import qualified Text.HeX.Math.MathML as MathML
import qualified Text.HeX.Math.LaTeX as LaTeXMath
import qualified Text.HeX.Math as Math

defaults :: HeX ()
defaults = do
  setVar "secnum" ([] :: [Int])
  Html.defaults
  LaTeX.defaults
  Generic.defaults
  Math.defaults
  MathML.mathFor "html"
  LaTeXMath.mathFor "latex"

