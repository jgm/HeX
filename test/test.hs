{-# LANGUAGE PackageImports, OverloadedStrings #-}
import Text.HeX
import Text.HeX.Standard as Standard
import Docbook as Docbook

main = defaultMain $ do
  Standard.commands
  Docbook.commands
  parseDoc

