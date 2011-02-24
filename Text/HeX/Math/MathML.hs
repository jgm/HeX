{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Text.HeX.Math.MathML (defaults) where

import Text.HeX
import Text.HeX.Standard.Xml
import Control.Applicative ((<$>))
import Text.HeX.Math (defaultsFor, withText)

defaults :: HeX ()
defaults = do
  defaultsFor writer
  register "textrm" $ asText "normal" <$> withText
  register "text" $ asText "normal" <$> withText
  register "mathrm" $ asText "normal" <$> group
  register "mbox" $ asText "normal" <$> group
  register "mathit" $ asText "italic" <$> group
  register "textit" $ asText "italic" <$> withText
  register "mathtt" $ asText "monospace" <$> group
  register "texttt" $ asText "monospace" <$> withText
  register "mathsf" $ asText "sans-serif" <$> group
  register "mathbb" $ asText "double-struck" <$> group
  register "mathcal" $ asText "script" <$> group
  register "mathfrak" $ asText "fraktur" <$> group
  register "sqrt" root
  register "surd" root

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

root :: Maybe Doc -> Doc -> Doc
root Nothing y  = inTags "mrow" [] $ inTags "msqrt" [] $ inTags "mn" [] y
root (Just x) y = inTags "mrow" [] $ inTags "mroot" []
                $ inTags "mn" [] y +++ inTags "mn" [] x

showOp :: String -> Doc
showOp s = inTags "mo" []
         $ case s of
              "'"    -> raws "\x02B9"
              "''"   -> raws "\x02BA"
              "'''"  -> raws "\x2034"
              "''''" -> raws "\x2057"
              _      -> raws s

asText :: String -> Doc -> Doc
asText variant = inTags "mtext" [("mathvariant",variant)]


