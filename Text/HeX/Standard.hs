{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, PackageImports #-}
module Text.HeX.Standard ( addCommands ) where

import Text.HeX
import Text.HeX.TeX as TeX
import Text.HeX.Html as Html
import Data.List (intercalate)
import "mtl" Control.Monad.Trans (liftIO)
import Control.Monad (forM_)

emitMathHtml, emitMathLaTeX :: Bool -> Doc -> HeX Doc
emitMathHtml display b = do
  let tagtype = if display then "div" else "span"
  return $ Html.inTags tagtype [("class","math")] b
emitMathLaTeX display b = do
  let delim = if display then "$$" else "$"
  return $ raws delim +++ b +++ raws delim

emphHtml, emphLaTeX :: Doc -> Doc
emphHtml arg  = inTags "em" [] arg
emphLaTeX arg  = ctl "emph" +++ grp [arg]

strongHtml, strongLaTeX :: Doc -> Doc
strongHtml arg  = inTags "strong" [] arg
strongLaTeX arg  = ctl "textbf" +++ grp [arg]

rpt :: Maybe Int -> Doc -> Doc
rpt (Just n) d = mconcat $ replicate n d
rpt Nothing  d = d

rev :: [Doc] -> Doc
rev = mconcat . reverse

include :: FilePath -> HeX Doc
include f = do
  contents <- liftIO $ readFile f
  rest <- getInput
  setInput $ contents ++ rest
  return mempty

test :: Maybe FilePath -> HeX Doc
test (Just f) = include f
test (Nothing) = return mempty

getSectionNum :: Int -> HeX String
getSectionNum lev = do
  (secnum :: [Int]) <- getVar "secnum"
  let thisnum = take lev $ zipWith (+) (secnum ++ repeat 0)
                                       (replicate (lev - 1) 0 ++ [1])
  setVar "secnum" thisnum
  let num = intercalate "." $ map show thisnum
  setTarget num
  return num

sectionHtml, sectionLaTeX :: Int -> Doc -> HeX Doc
sectionHtml lev d = do
  num <- getSectionNum lev
  return $ inTags ("h" ++ show lev) [] (raws num +++ ". " +++ d)

sectionLaTeX lev d = do
  _ <- getSectionNum lev  -- we need to increment the number
  let secheading = case lev of
                        1 -> "section"
                        2 -> "subsection"
                        3 -> "subsubsection"
                        4 -> "paragraph"
                        _ -> "subparagraph"
  return $ ctl secheading +++ grp [d]

label' :: String -> HeX Doc
label' s = addLabel s >> return mempty

ref :: String -> HeX Doc
ref s = lookupLabel s

addCommands :: HeX ()
addCommands = do
  setVar "secnum" ([] :: [Int])
  registerEscaperFor "html" (return . Html.ch)
  registerEscaperFor "latex" (return . TeX.ch)
  registerEmitMathFor "html" emitMathHtml
  registerEmitMathFor "latex" emitMathLaTeX
  registerFor "html" "emph" emphHtml
  registerFor "latex" "emph" emphLaTeX
  registerFor "html" "strong" strongHtml
  registerFor "latex" "strong" strongLaTeX
  register "rpt" rpt
  register "rev" rev
  register "include" include
  register "test" test
  forM_ [("html",sectionHtml),("latex",sectionLaTeX)] $ \(format,fn) -> do
    registerFor format "section" (fn 1)
    registerFor format "subsection" (fn 2)
    registerFor format "subsubsection" (fn 3)
    registerFor format "paragraph" (fn 4)
    registerFor format "subparagraph" (fn 5)
  register "label" label'
  register "ref" ref
