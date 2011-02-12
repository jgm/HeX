{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Text.HeX.Standard ( addCommands ) where

import Text.HeX
import Text.HeX.TeX as TeX
import Text.HeX.Html as Html
import Data.List (intercalate)
import Control.Monad.Trans (liftIO)

emph :: Format -> Doc -> Doc
emph Html arg  = inTags "em" [] arg
emph LaTeX arg  = ctl "emph" +++ grp [arg]

strong :: Format -> Doc -> Doc
strong Html arg  = inTags "strong" [] arg
strong LaTeX arg  = ctl "textbf" +++ grp [arg]

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
           Html    -> inTags ("h" ++ show lev) [] (raws num +++ ". " +++ d)
           LaTeX   -> ctl secheading +++ grp [d]

label' :: String -> HeX Doc
label' s = addLabel s >> return mempty

ref :: String -> HeX Doc
ref s = lookupLabel s

addCommands :: HeX ()
addCommands = do
  setVar "secnum" ([] :: [Int])
  addCommand "emph" emph
  addCommand "strong" strong
  addCommand "rpt"  rpt
  addCommand "rev"  rev
  addCommand "include" include
  addCommand "test" test
  addCommand "section" (section 1)
  addCommand "subsection" (section 2)
  addCommand "subsubsection" (section 3)
  addCommand "paragraph" (section 4)
  addCommand "subparagraph" (section 5)
  addCommand "label" label'
  addCommand "ref" ref
