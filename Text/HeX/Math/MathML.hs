{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Text.HeX.Math.MathML (mathFor) where

import Text.HeX
import Text.HeX.Standard.Xml
import Control.Applicative ((<$>))
import Text.HeX.Math (withText)

mathFor :: Format -> HeX ()
mathFor f = do
  registerMathWriterFor f writer
  registerFor f "textrm" $ asText "normal" <$> withText
  registerFor f "text" $ asText "normal" <$> withText
  registerFor f "mathrm" $ asText "normal" <$> group
  registerFor f "mbox" $ asText "normal" <$> group
  registerFor f "mathit" $ asText "italic" <$> group
  registerFor f "textit" $ asText "italic" <$> withText
  registerFor f "mathtt" $ asText "monospace" <$> group
  registerFor f "texttt" $ asText "monospace" <$> withText
  registerFor f "mathsf" $ asText "sans-serif" <$> group
  registerFor f "mathbb" $ asText "double-struck" <$> group
  registerFor f "mathcal" $ asText "script" <$> group
  registerFor f "mathfrak" $ asText "fraktur" <$> group

writer :: MathWriter
writer = MathWriter{
   mathFormat = "mathml"
 , displayMath = \d ->
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

asText :: String -> Doc -> Doc
asText variant = inTags "mtext" [("mathvariant",variant)]


