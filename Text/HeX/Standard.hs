{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, PackageImports #-}
module Text.HeX.Standard ( addCommands ) where

import Text.HeX
import Text.HeX.TeX as TeX
import Text.HeX.Html as Html
import Data.List (intercalate)
import "mtl" Control.Monad.Trans (liftIO)
import Control.Monad (forM_)

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

section :: Int -> Format -> Doc -> HeX Doc
section lev f d = do
  (secnum :: [Int]) <- getVar "secnum"
  let thisnum = take lev $ zipWith (+) (secnum ++ repeat 0)
                                       (replicate (lev - 1) 0 ++ [1])
  setVar "secnum" thisnum
  let num = intercalate "." $ map show thisnum
  setTarget num
  let secheading = case lev of
                        1 -> "section"
                        2 -> "subsection"
                        3 -> "subsubsection"
                        4 -> "paragraph"
                        _ -> "subparagraph"
  return $ case f of
           "html"    -> inTags ("h" ++ show lev) [] (raws num +++ ". " +++ d)
           "latex"   -> ctl secheading +++ grp [d]

label' :: String -> HeX Doc
label' s = addLabel s >> return mempty

ref :: String -> HeX Doc
ref s = lookupLabel s

addCommands :: HeX ()
addCommands = do
  setVar "secnum" ([] :: [Int])
  registerEscaperFor "html" (return . Html.ch)
  registerEscaperFor "latex" (return . TeX.ch)
  registerFor "html" "emph" emphHtml
  registerFor "latex" "emph" emphLaTeX
  registerFor "html" "strong" strongHtml
  registerFor "latex" "strong" strongLaTeX
  register "rpt" rpt
  register "rev" rev
  register "include" include
  register "test" test
  forM_ ["html","latex"] $ \f -> do
    registerFor f "section" (section 1 f)
    registerFor f "subsection" (section 2 f)
    registerFor f "subsubsection" (section 3 f)
    registerFor f "paragraph" (section 4 f)
    registerFor f "subparagraph" (section 5 f)
  register "label" label'
  register "ref" ref
