{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Text.HeX.Math.MathML (mathFor) where

import Text.HeX
import Text.HeX.Standard.Xml
import Control.Applicative ((<$>))
import Text.HeX.Math (withText)

mathFor :: Format -> HeX ()
mathFor f = do
  registerMathWriterFor f writer
  registerFor "mathml" "textrm" $ asText "normal" <$> withText
  registerFor "mathml" "text" $ asText "normal" <$> withText
  registerFor "mathml" "mathrm" $ asText "normal" <$> group
  registerFor "mathml" "mbox" $ asText "normal" <$> group
  registerFor "mathml" "mathit" $ asText "italic" <$> group
  registerFor "mathml" "textit" $ asText "italic" <$> withText
  registerFor "mathml" "mathtt" $ asText "monospace" <$> group
  registerFor "mathml" "texttt" $ asText "monospace" <$> withText
  registerFor "mathml" "mathsf" $ asText "sans-serif" <$> group
  registerFor "mathml" "mathbb" $ asText "double-struck" <$> group
  registerFor "mathml" "mathcal" $ asText "script" <$> group
  registerFor "mathml" "mathfrak" $ asText "fraktur" <$> group

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


