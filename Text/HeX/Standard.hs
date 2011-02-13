{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, PackageImports #-}
module Text.HeX.Standard ( commands ) where

import Text.HeX
import qualified Text.HeX.Standard.Generic as Generic
import qualified Text.HeX.Standard.Html as Html
import qualified Text.HeX.Standard.LaTeX as LaTeX
import qualified Text.HeX.Math.Html as HtmlMath
import qualified Text.HeX.Math.LaTeX as LaTeXMath

commands :: HeX ()
commands = do
  setVar "secnum" ([] :: [Int])
  HtmlMath.commands
  LaTeXMath.commands
  Html.commands
  LaTeX.commands
  Generic.commands

