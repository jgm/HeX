{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Text.HeX.Math.Html (commands) where

import Text.HeX
import Text.HeX.Standard.Xml (inTags)
import Text.HeX.Math.Generic (math)

commands :: HeX ()
commands = do
  addParser (math parseToken emitMath)

parseToken :: HeX Doc
parseToken = oneChar -- TODO

emitMath :: Bool -> Doc -> HeX Doc
emitMath display b = do
  let tagtype = if display then "div" else "span"
  return $ inTags tagtype [("class","math")] b

