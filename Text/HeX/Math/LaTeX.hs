{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Text.HeX.Math.LaTeX (defaults) where

import Text.HeX
import Text.HeX.Standard.TeX
import Control.Applicative ((<$>))
import Text.HeX.Math (defaultsFor, withText)

defaults :: HeX ()
defaults = do
  defaultsFor writer
  register "textrm" $ inCtl "textrm" <$> withText
  register "text" $ inCtl "text" <$> withText
  register "textit" $ inCtl "textit" <$> withText
  register "texttt" $ inCtl "texttt" <$> withText
  mapM_ latexCommand [ "mathrm"
                     , "mbox"
                     , "mathit"
                     , "mathtt"
                     , "mathsf"
                     , "mathbb"
                     , "mathcal"
                     , "mathfrak" ]
  register "sqrt" root
  register "surd" root
  mapM_ latexCommand [ "acute"
                     , "grave"
                     , "breve"
                     , "check"
                     , "dot"
                     , "ddot"
                     , "mathring"
                     , "vec"
                     , "overrightarrow"
                     , "overleftarrow"
                     , "hat"
                     , "widehat"
                     , "tilde"
                     , "widetilde"
                     , "bar"
                     , "overbrace"
                     , "overbracket"
                     , "overline"
                     , "underbrace"
                     , "underbracket"
                     , "underline" ]

latexCommand :: String -> HeX ()
latexCommand s = register s $ inCtl s <$> getNext

inCtl :: String -> Doc -> Doc
inCtl s d = ctl s +++ d

root :: Maybe Doc -> Doc -> Doc
root x y = "\\sqrt" +++ x' +++ y
  where x' = case x of
                  Just z  -> "[" +++ z +++ "]"
                  Nothing -> mempty

writer :: MathWriter
writer = MathWriter{
   mathFormat = "latex"
 , displayMath = \d ->
     "$$" +++ d +++ "$$"
 , inlineMath = \d ->
     "$" +++ d +++ "$"
 , grouped = \d -> "{" +++ d +++ "}"
 , variable = rawc
 , number = raws
 , operator = raws
 }
