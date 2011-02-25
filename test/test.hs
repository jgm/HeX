{-# LANGUAGE PackageImports, OverloadedStrings #-}
import Text.HeX
import Text.HeX.Standard as Standard
import Docbook as Docbook
import Control.Monad (guard)
import Text.HeX.Standard.Xml (inTags)
import Text.HeX.Standard.TeX (ctl,grp)

main = defaultMain $ do
  Standard.defaults
  forFormat "docbook" Docbook.defaults
  register "silly" silly
  register "lettrine" lettrine
  parseDoc

silly :: OptionList -> Doc
silly (OptionList opts) =
  raws $ show opts

lettrine :: Format -> Doc -> Doc -> Doc
lettrine "html" x y = inTags "span" [("class","lettrine")] x +++ y
lettrine "latex" x y = ctl "lettrine" +++ grp [x] +++ grp [y]

-- CSS:
-- body { line-height: 1.3; }
-- .lettrine {font-size:3em; float: left; line-height: 1; margin-right: 0.1em;}

