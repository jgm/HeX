{-# LANGUAGE OverloadedStrings #-}
import Text.HeX.Default
import Text.HeX.TeX as TeX
import Text.HeX.Html as Html

emph :: Format -> Doc -> Doc
emph "html" arg  = inTags "em" [] arg
emph "tex"  arg  = ctl "emph" +++ grp [arg]

name :: Format -> Doc -> Doc -> Doc
name "html" f l = inTags "span" [("class","firstname")] f +++ " "
                   +++ inTags "span" [("class","lastname")] l

rpt :: Maybe Int -> Doc -> Doc
rpt (Just n) d = mconcat $ replicate n d
rpt Nothing  d = d

main = defaultMain [
            "emph" =: emph
          , "name" =: name
          , "rpt"  =: rpt
          ]

