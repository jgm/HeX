{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Text.HeX.Math.MathML (writer) where

import Text.HeX
import Text.HeX.Standard.Xml (ch, inTags)
import Control.Monad (liftM)

writer :: MathWriter
writer = MathWriter{
   displayMath = mathenv True
 , inlineMath  = mathenv False
 , grouped = inTags "mrow" []
 }

mathenv :: Bool -> HeX Doc -> HeX Doc
mathenv display p = do
  res <- p
  return $ if display
              then inTags "div" [("class","math")] res
              else inTags "span" [("class","math")] res
