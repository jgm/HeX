{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Text.HeX.Math.MathML (mathFor) where

import Text.HeX
import Text.HeX.Standard.Xml
import Control.Monad (liftM)

mathFor :: Format -> HeX ()
mathFor f = do
  registerMathWriterFor f writer

writer :: MathWriter
writer = MathWriter{
   displayMath = \d ->
     inTags "math" [("display","block"), ("xmlns",xmlns)] $ inTags "mrow" [] d
 , inlineMath = \d ->
     inTags "math" [("display","inline"), ("xmlns",xmlns)] $ inTags "mrow" [] d
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

