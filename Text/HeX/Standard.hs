{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, PackageImports #-}
module Text.HeX.Standard ( defaults ) where

import Text.HeX
import qualified Text.HeX.Standard.Generic as Generic
import qualified Text.HeX.Standard.Html as Html
import qualified Text.HeX.Standard.LaTeX as LaTeX
import qualified Text.HeX.Math.MathML as MathML
import qualified Text.HeX.Math.LaTeX as LaTeXMath

defaults :: HeX ()
defaults = do
  setVar "secnum" ([] :: [Int])
  format <- getFormat
  case format of
    "html"  -> Html.defaults >> MathML.defaults
    "latex" -> LaTeX.defaults >> LaTeXMath.defaults
    _       -> return ()
  Generic.defaults

