{-# LANGUAGE PackageImports, OverloadedStrings #-}
module Text.HeX.Standard ( addCommands ) where

import Text.HeX
import Text.HeX.TeX as TeX
import Text.HeX.Html as Html
import "mtl" Control.Monad.Trans (liftIO)

emph :: Format -> Doc -> Doc
emph Html arg  = inTags "em" [] arg
emph LaTeX arg  = ctl "emph" +++ grp [arg]

name :: Format -> Doc -> Doc -> Doc
name Html f l = inTags "span" [("class","firstname")] f +++ " "
                   +++ inTags "span" [("class","lastname")] l

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

section :: Format -> Maybe Integer -> Doc -> HeX Doc
section f n d = do
  d' <- case n of
             Just x -> do
               setTarget (show x)
               return (raws (show x) +++ ". " +++ d)
             _      -> return d
  return $ case f of
           Html    -> inTags "h1" [] d'
           LaTeX   -> ctl "section" +++ grp [d']

lab :: String -> HeX Doc
lab s = addLabel s >> return mempty

ref :: String -> HeX Doc
ref s = lookupLabel s

addCommands :: HeX ()
addCommands = do
  addCommand "emph" emph
  addCommand "name" name
  addCommand "rpt"  rpt
  addCommand "rev"  rev
  addCommand "include" include
  addCommand "test" test
  addCommand "section" section
  addCommand "lab" lab
  addCommand "ref" ref
