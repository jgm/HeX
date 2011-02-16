{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Text.HeX.Math.MathML (writer, commands) where

import Text.HeX
import Text.HeX.Standard.Xml (ch, inTags)
import Text.HeX.Math.Generic (math)
import Control.Monad (liftM)

commands :: HeX ()
commands = do
  registerEscaperFor "mathml" (return . ch)
  addParser (math writer)

writer :: MathWriter
writer = MathWriter{
   displayMath = mathenv True
 , inlineMath  = mathenv False
 , grouped = inTags "mrow" []
 }

mathenv :: Bool -> HeX Doc -> HeX Doc
mathenv display p = do
  oldformat <- liftM hexFormat getState
  updateState $ \st -> st{ hexFormat = "mathml" }
  res <- p
  updateState $ \st -> st{ hexFormat = oldformat }
  return $ if display
              then inTags "div" [("class","math")] res
              else inTags "span" [("class","math")] res
