{-# LANGUAGE PackageImports, OverloadedStrings #-}
import Text.HeX.Default
import Text.HeX.TeX as TeX
import Text.HeX.Html as Html
import "mtl" Control.Monad.Trans (liftIO)

emph :: Format -> Doc -> Doc
emph Html arg  = inTags "em" [] arg
emph LaTeX arg  = ctl "emph" +++ grp [arg]

name :: Format -> Doc -> Doc -> Doc
name Html f l = inTags "span" [("class","firstname")] f +++ " "
                   +++ inTags "span" [("class","lastname")] l

rpt :: Maybe Int -> Doc -> Doc
rpt (Just n) d = mconcat $ replicate n d
rpt Nothing  d = d

include :: FilePath -> HeX Doc
include f = do
  contents <- liftIO $ readFile f
  rest <- getInput
  setInput $ contents ++ rest
  return mempty

main = defaultMain [
            "emph" =: emph
          , "name" =: name
          , "rpt"  =: rpt
          , "include" =: include
          ]

