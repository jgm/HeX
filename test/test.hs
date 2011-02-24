{-# LANGUAGE PackageImports, OverloadedStrings #-}
import Text.HeX
import Text.HeX.Standard as Standard
import Docbook as Docbook

main = defaultMain $ do
  Standard.defaults
  Docbook.defaults
  register "silly" silly
  parseDoc


silly :: OptionList -> Doc
silly (OptionList opts) =
  raws $ show opts
