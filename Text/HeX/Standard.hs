{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, PackageImports #-}
module Text.HeX.Standard ( commands ) where

import Text.HeX
import qualified Text.HeX.Standard.Generic as Generic
import qualified Text.HeX.Standard.Html as Html
import qualified Text.HeX.Standard.LaTeX as LaTeX

commands :: HeX ()
commands = do
  setVar "secnum" ([] :: [Int])
  Html.commands
  LaTeX.commands
  Generic.commands

