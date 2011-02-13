{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Text.HeX.Math.Html (commands, emitMath) where

import Text.HeX
import Text.HeX.Standard.Xml (inTags)

commands :: HeX ()
commands = do
  registerEmitMathFor "html" emitMath

emitMath :: Bool -> Doc -> HeX Doc
emitMath display b = do
  let tagtype = if display then "div" else "span"
  return $ inTags tagtype [("class","math")] b

