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
 , variable = inTags "mi" []
 , number = inTags "mn" []
 }

mathenv :: Bool -> HeX Doc -> HeX Doc
mathenv display p = do
  res <- p
  let xmlns = "http://www.w3.org/1998/Math/MathML"
  let res' = inTags "mrow" [] res
  return $ if display
              then inTags "math" [("display","block"),
                                  ("xmlns",xmlns)] res'
              else inTags "math" [("display","inline"),
                                  ("xmlns",xmlns)] res'
