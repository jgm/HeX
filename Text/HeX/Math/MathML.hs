{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Text.HeX.Math.MathML (writer, commands) where

import Text.HeX
import Text.HeX.Standard.Xml (inTags)
import Text.HeX.Math.Generic (math, MathWriter(..))

commands :: HeX ()
commands = do
  addParser (math writer)

writer :: MathWriter
writer = MathWriter{
   displayMath = display
 , inlineMath  = inline
 , grouped = inTags "mrow" []
 }

display :: Doc -> Doc
display = inTags "div" [("class","math")]

inline :: Doc -> Doc
inline = inTags "span" [("class","math")]


