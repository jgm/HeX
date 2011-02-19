{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Text.HeX.Math.MathML (writer) where

import Text.HeX
import Text.HeX.Standard.Xml
import Control.Monad (liftM)

writer :: MathWriter
writer = MathWriter{
   startDisplayMath = tagOpen "math" [("display","block"), ("xmlns",xmlns)] +++
                      tagOpen "mrow" []
 , endDisplayMath = tagClose "mrow" +++ tagClose "math"
 , startInlineMath = tagOpen "math" [("display","block"), ("xmlns",xmlns)] +++
                     tagOpen "mrow" []
 , endInlineMath = tagClose "mrow" +++ tagClose "math"
 , grouped = inTags "mrow" []
 , variable = inTags "mi" [] . rawc
 , number = inTags "mn" [] . raws
 , operator = showOp
 }

xmlns :: String
xmlns = "http://www.w3.org/1998/Math/MathML"

showOp :: String -> Doc
showOp "'" = inTags "mo" [] $ raws "\x02B9"
showOp "''" = inTags "mo" [] $ raws "\x02BA"
showOp "'''" = inTags "mo" [] $ raws "\x2034"
showOp "''''" = inTags "mo" [] $ raws "\x2057"
showOp s = inTags "mo" [] $ raws s

