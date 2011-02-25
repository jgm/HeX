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
  register "mathrm" $ asText "normal" <$> getNext
  register "mbox" $ asText "normal" <$> getNext
  register "mathit" $ asText "italic" <$> getNext
  register "textit" $ asText "italic" <$> withText
  register "mathtt" $ asText "monospace" <$> getNext
  register "texttt" $ asText "monospace" <$> withText
  register "mathsf" $ asText "sans-serif" <$> getNext
  register "mathbb" $ asText "double-struck" <$> getNext
  register "mathcal" $ asText "script" <$> getNext
  register "mathfrak" $ asText "fraktur" <$> getNext
  register "sqrt" root
  register "surd" root
  register "acute" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#180;"
  register "grave" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "`"
  register "breve" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#728;"
  register "check" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#711;"
  register "dot" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "."
  register "ddot" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] ".."
  register "mathring" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#176;"
  register "vec" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#8407;"
  register "overrightarrow" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#8407;"
  register "overleftarrow" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#8406;"
  register "hat" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "^"
  register "widehat" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#770;"
  register "tilde" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "~"
  register "widetilde" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#732;"
  register "bar" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#8254;"
  register "overbrace" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#65079;"
  register "overbracket" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#9140;"
  register "overline" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#175;"
  register "underbrace" $ \d ->
    inTags "munder" [] $ d +++ inTags "mo" [("accent","true")] "&#65080;"
  register "underbracket" $ \d ->
    inTags "munder" [] $ d +++ inTags "mo" [("accent","true")] "&#9141;"
  register "underline" $ \d ->
    inTags "munder" [] $ d +++ inTags "mo" [("accent","true")] "&#175;"

writer :: MathWriter
writer = MathWriter{
   mathFormat = "mathml"
 , displayMath = \d ->
     inTags "math" [("display","block"), ("xmlns",xmlns)] $ mrow d
 , inlineMath = \d ->
     inTags "math" [("display","inline"), ("xmlns",xmlns)] $ mrow d
 , grouped = mrow
 , variable = inTags "mi" [] . rawc
 , number = inTags "mn" [] . raws
 , operator = showOp
 }

mrow :: Doc -> Doc
mrow = inTags "mrow" []

mover :: Doc -> Doc
mover = inTags "mover" []

xmlns :: String
xmlns = "http://www.w3.org/1998/Math/MathML"

root :: Maybe Doc -> Doc -> Doc
root Nothing y  = inTags "msqrt" [] $ inTags "mn" [] y
root (Just x) y = inTags "mroot" [] $ inTags "mn" [] y +++ inTags "mn" [] x

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


