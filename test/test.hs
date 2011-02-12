{-# LANGUAGE PackageImports, OverloadedStrings #-}
import Text.HeX
import Text.HeX.Standard (addCommands)

main = defaultMain $ do
  addCommands
  parseDoc

