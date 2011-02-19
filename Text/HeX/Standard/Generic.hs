{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, PackageImports #-}
module Text.HeX.Standard.Generic (defaults, getSectionNum) where

import Text.HeX
import Data.List (intercalate)
import "mtl" Control.Monad.Trans (liftIO)

defaults :: HeX ()
defaults = do
  register "rpt" rpt
  register "rev" rev
  register "include" include
  register "label" label'
  register "ref" ref

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

getSectionNum :: Int -> HeX String
getSectionNum lev = do
  (secnum :: [Int]) <- getVar "secnum"
  let thisnum = take lev $ zipWith (+) (secnum ++ repeat 0)
                                       (replicate (lev - 1) 0 ++ [1])
  setVar "secnum" thisnum
  let num = intercalate "." $ map show thisnum
  setTarget num
  return num

label' :: String -> HeX Doc
label' s = addLabel s >> return mempty

ref :: String -> HeX Doc
ref s = lookupLabel s

