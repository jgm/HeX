{-# LANGUAGE PackageImports, OverloadedStrings #-}
import Text.HeX
import Text.HeX.Standard (commands)

main = defaultMain $ do
  commands
  parseDoc

