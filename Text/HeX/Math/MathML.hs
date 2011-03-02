{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Text.HeX.Math.MathML (defaults) where

import Text.HeX
import Text.HeX.Standard.Xml
import Control.Applicative ((<$>))
import Text.HeX.Math (defaultsFor)
import qualified Data.Map as M
import Data.ByteString.Lazy.UTF8 (toString)
import Blaze.ByteString.Builder
import Data.List (isPrefixOf)

defaults :: HeX ()
defaults = do
  defaultsFor writer
  addParser [Math] enclosure
  updateState $ \st -> st{ hexParsers =
     M.adjust (\xs -> [subsup xs]) Math $ hexParsers st }
  register [Math] "textrm" $ asText "normal" <$> inline
  register [Math] "text" $ asText "normal" <$> inline
  register [Math] "mathrm" $ asText "normal" <$> math
  register [Math] "mbox" $ asText "normal" <$> math
  register [Math] "mathit" $ asText "italic" <$> math
  register [Math] "textit" $ asText "italic" <$> inline
  register [Math] "mathtt" $ asText "monospace" <$> math
  register [Math] "texttt" $ asText "monospace" <$> inline
  register [Math] "mathsf" $ asText "sans-serif" <$> math
  register [Math] "mathbb" $ asText "double-struck" <$> math
  register [Math] "mathcal" $ asText "script" <$> math
  register [Math] "mathfrak" $ asText "fraktur" <$> math
  register [Math] "sqrt" root
  register [Math] "surd" root
  register [Math] "acute" $ \(MathDoc d) ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#180;"
  register [Math] "grave" $ \(MathDoc d) ->
    mover $ d +++ inTags "mo" [("accent","true")] "`"
  register [Math] "breve" $ \(MathDoc d) ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#728;"
  register [Math] "check" $ \(MathDoc d) ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#711;"
  register [Math] "dot" $ \(MathDoc d) ->
    mover $ d +++ inTags "mo" [("accent","true")] "."
  register [Math] "ddot" $ \(MathDoc d) ->
    mover $ d +++ inTags "mo" [("accent","true")] ".."
  register [Math] "mathring" $ \(MathDoc d) ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#176;"
  register [Math] "vec" $ \(MathDoc d) ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#8407;"
  register [Math] "overrightarrow" $ \(MathDoc d) ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#8407;"
  register [Math] "overleftarrow" $ \(MathDoc d) ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#8406;"
  register [Math] "hat" $ \(MathDoc d) ->
    mover $ d +++ inTags "mo" [("accent","true")] "^"
  register [Math] "widehat" $ \(MathDoc d) ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#770;"
  register [Math] "tilde" $ \(MathDoc d) ->
    mover $ d +++ inTags "mo" [("accent","true")] "~"
  register [Math] "widetilde" $ \(MathDoc d) ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#732;"
  register [Math] "bar" $ \(MathDoc d) ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#8254;"
  register [Math] "overbrace" $ \(MathDoc d) ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#65079;"
  register [Math] "overbracket" $ \(MathDoc d) ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#9140;"
  register [Math] "overline" $ \(MathDoc d) ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#175;"
  register [Math] "underbrace" $ \(MathDoc d) ->
    inTags "munder" [] $ d +++ inTags "mo" [("accent","true")] "&#65080;"
  register [Math] "underbracket" $ \(MathDoc d) ->
    inTags "munder" [] $ d +++ inTags "mo" [("accent","true")] "&#9141;"
  register [Math] "underline" $ \(MathDoc d) ->
    inTags "munder" [] $ d +++ inTags "mo" [("accent","true")] "&#175;"
  register [Math] "mid" $ inTags "mo" [] "&#x2223;"
  register [Math] "parallel" $ inTags "mo" [] "&#x2225;"
  register [Math] "backslash" $ inTags "mo" [] "&#x2216;"
  register [Math] "times" $ inTags "mo" [] "&#x00D7;"
  register [Math] "alpha" $ inTags "mo" [] "&#x03B1;"
  register [Math] "beta" $ inTags "mo" [] "&#x03B2;"
  register [Math] "chi" $ inTags "mo" [] "&#x03C7;"
  register [Math] "delta" $ inTags "mo" [] "&#x03B4;"
  register [Math] "Delta" $ inTags "mo" [] "&#x0394;"
  register [Math] "epsilon" $ inTags "mo" [] "&#x03B5;"
  register [Math] "varepsilon" $ inTags "mo" [] "&#x025B;"
  register [Math] "eta" $ inTags "mo" [] "&#x03B7;"
  register [Math] "gamma" $ inTags "mo" [] "&#x03B3;"
  register [Math] "Gamma" $ inTags "mo" [] "&#x0393;" 
  register [Math] "iota" $ inTags "mo" [] "&#x03B9;"
  register [Math] "kappa" $ inTags "mo" [] "&#x03BA;"
  register [Math] "lambda" $ inTags "mo" [] "&#x03BB;"
  register [Math] "Lambda" $ inTags "mo" [] "&#x039B;" 
  register [Math] "mu" $ inTags "mo" [] "&#x03BC;"
  register [Math] "nu" $ inTags "mo" [] "&#x03BD;"
  register [Math] "omega" $ inTags "mo" [] "&#x03C9;"
  register [Math] "Omega" $ inTags "mo" [] "&#x03A9;"
  register [Math] "phi" $ inTags "mo" [] "&#x03C6;"
  register [Math] "varphi" $ inTags "mo" [] "&#x03D5;"
  register [Math] "Phi" $ inTags "mo" [] "&#x03A6;" 
  register [Math] "pi" $ inTags "mo" [] "&#x03C0;"
  register [Math] "Pi" $ inTags "mo" [] "&#x03A0;" 
  register [Math] "psi" $ inTags "mo" [] "&#x03C8;"
  register [Math] "Psi" $ inTags "mo" [] "&#x03A8;"
  register [Math] "rho" $ inTags "mo" [] "&#x03C1;"
  register [Math] "sigma" $ inTags "mo" [] "&#x03C3;"
  register [Math] "Sigma" $ inTags "mo" [] "&#x03A3;" 
  register [Math] "tau" $ inTags "mo" [] "&#x03C4;"
  register [Math] "theta" $ inTags "mo" [] "&#x03B8;"
  register [Math] "vartheta" $ inTags "mo" [] "&#x03D1;"
  register [Math] "Theta" $ inTags "mo" [] "&#x0398;" 
  register [Math] "upsilon" $ inTags "mo" [] "&#x03C5;"
  register [Math] "xi" $ inTags "mo" [] "&#x03BE;"
  register [Math] "Xi" $ inTags "mo" [] "&#x039E;" 
  register [Math] "zeta" $ inTags "mo" [] "&#x03B6;"
  register [Math] "frac12" $ inTags "mo" [] "&#x00BD;"
  register [Math] "frac14" $ inTags "mo" [] "&#x00BC;"
  register [Math] "frac34" $ inTags "mo" [] "&#x00BE;"
  register [Math] "frac13" $ inTags "mo" [] "&#x2153;"
  register [Math] "frac23" $ inTags "mo" [] "&#x2154;"
  register [Math] "frac15" $ inTags "mo" [] "&#x2155;"
  register [Math] "frac25" $ inTags "mo" [] "&#x2156;"
  register [Math] "frac35" $ inTags "mo" [] "&#x2157;"
  register [Math] "frac45" $ inTags "mo" [] "&#x2158;"
  register [Math] "frac16" $ inTags "mo" [] "&#x2159;"
  register [Math] "frac56" $ inTags "mo" [] "&#x215A;"
  register [Math] "frac18" $ inTags "mo" [] "&#x215B;"
  register [Math] "frac38" $ inTags "mo" [] "&#x215C;"
  register [Math] "frac58" $ inTags "mo" [] "&#x215D;"
  register [Math] "frac78" $ inTags "mo" [] "&#x215E;"
  register [Math] "pm" $ inTags "mo" [] "&#x00B1;"
  register [Math] "mp" $ inTags "mo" [] "&#x2213;"
  register [Math] "triangleleft" $ inTags "mo" [] "&#x22B2;"
  register [Math] "triangleright" $ inTags "mo" [] "&#x22B3;"
  register [Math] "cdot" $ inTags "mo" [] "&#x22C5;"
  register [Math] "star" $ inTags "mo" [] "&#x22C6;"
  register [Math] "ast" $ inTags "mo" [] "&#x002A;"
  register [Math] "times" $ inTags "mo" [] "&#x00D7;"
  register [Math] "div" $ inTags "mo" [] "&#x00F7;"
  register [Math] "circ" $ inTags "mo" [] "&#x2218;"
  register [Math] "bullet" $ inTags "mo" [] "&#x2022;"
  register [Math] "oplus" $ inTags "mo" [] "&#x2295;"
  register [Math] "ominus" $ inTags "mo" [] "&#x2296;"
  register [Math] "otimes" $ inTags "mo" [] "&#x2297;"
  register [Math] "bigcirc" $ inTags "mo" [] "&#x25CB;"
  register [Math] "oslash" $ inTags "mo" [] "&#x2298;"
  register [Math] "odot" $ inTags "mo" [] "&#x2299;"
  register [Math] "land" $ inTags "mo" [] "&#x2227;"
  register [Math] "wedge" $ inTags "mo" [] "&#x2227;"
  register [Math] "lor" $ inTags "mo" [] "&#x2228;"
  register [Math] "vee" $ inTags "mo" [] "&#x2228;"
  register [Math] "cap" $ inTags "mo" [] "&#x2229;"
  register [Math] "cup" $ inTags "mo" [] "&#x222A;"
  register [Math] "sqcap" $ inTags "mo" [] "&#x2293;"
  register [Math] "sqcup" $ inTags "mo" [] "&#x2294;"
  register [Math] "uplus" $ inTags "mo" [] "&#x228E;"
  register [Math] "amalg" $ inTags "mo" [] "&#x2210;"
  register [Math] "bigtriangleup" $ inTags "mo" [] "&#x25B3;"
  register [Math] "bigtriangledown" $ inTags "mo" [] "&#x25BD;"
  register [Math] "dag" $ inTags "mo" [] "&#x2020;"
  register [Math] "dagger" $ inTags "mo" [] "&#x2020;"
  register [Math] "ddag" $ inTags "mo" [] "&#x2021;"
  register [Math] "ddagger" $ inTags "mo" [] "&#x2021;"
  register [Math] "lhd" $ inTags "mo" [] "&#x22B2;"
  register [Math] "rhd" $ inTags "mo" [] "&#x22B3;"
  register [Math] "unlhd" $ inTags "mo" [] "&#x22B4;"
  register [Math] "unrhd" $ inTags "mo" [] "&#x22B5;"
  register [Math] "lt" $ inTags "mo" [] "<"
  register [Math] "gt" $ inTags "mo" [] ">"
  register [Math] "ne" $ inTags "mo" [] "&#x2260;"
  register [Math] "neq" $ inTags "mo" [] "&#x2260;"
  register [Math] "le" $ inTags "mo" [] "&#x2264;"
  register [Math] "leq" $ inTags "mo" [] "&#x2264;"
  register [Math] "leqslant" $ inTags "mo" [] "&#x2264;"
  register [Math] "ge" $ inTags "mo" [] "&#x2265;"
  register [Math] "geq" $ inTags "mo" [] "&#x2265;"
  register [Math] "geqslant" $ inTags "mo" [] "&#x2265;"
  register [Math] "equiv" $ inTags "mo" [] "&#x2261;"
  register [Math] "ll" $ inTags "mo" [] "&#x226A;"
  register [Math] "gg" $ inTags "mo" [] "&#x226B;"
  register [Math] "doteq" $ inTags "mo" [] "&#x2250;"
  register [Math] "prec" $ inTags "mo" [] "&#x227A;"
  register [Math] "succ" $ inTags "mo" [] "&#x227B;"
  register [Math] "preceq" $ inTags "mo" [] "&#x227C;"
  register [Math] "succeq" $ inTags "mo" [] "&#x227D;"
  register [Math] "subset" $ inTags "mo" [] "&#x2282;"
  register [Math] "supset" $ inTags "mo" [] "&#x2283;"
  register [Math] "subseteq" $ inTags "mo" [] "&#x2286;"
  register [Math] "supseteq" $ inTags "mo" [] "&#x2287;"
  register [Math] "sqsubset" $ inTags "mo" [] "&#x228F;"
  register [Math] "sqsupset" $ inTags "mo" [] "&#x2290;"
  register [Math] "sqsubseteq" $ inTags "mo" [] "&#x2291;"
  register [Math] "sqsupseteq" $ inTags "mo" [] "&#x2292;"
  register [Math] "sim" $ inTags "mo" [] "&#x223C;"
  register [Math] "simeq" $ inTags "mo" [] "&#x2243;"
  register [Math] "approx" $ inTags "mo" [] "&#x2248;"
  register [Math] "cong" $ inTags "mo" [] "&#x2245;"
  register [Math] "Join" $ inTags "mo" [] "&#x22C8;"
  register [Math] "bowtie" $ inTags "mo" [] "&#x22C8;"
  register [Math] "in" $ inTags "mo" [] "&#x2208;"
  register [Math] "ni" $ inTags "mo" [] "&#x220B;"
  register [Math] "owns" $ inTags "mo" [] "&#x220B;"
  register [Math] "propto" $ inTags "mo" [] "&#x221D;"
  register [Math] "vdash" $ inTags "mo" [] "&#x22A2;"
  register [Math] "dashv" $ inTags "mo" [] "&#x22A3;"
  register [Math] "models" $ inTags "mo" [] "&#x22A8;"
  register [Math] "perp" $ inTags "mo" [] "&#x22A5;"
  register [Math] "smile" $ inTags "mo" [] "&#x2323;"
  register [Math] "frown" $ inTags "mo" [] "&#x2322;"
  register [Math] "asymp" $ inTags "mo" [] "&#x224D;"
  register [Math] "notin" $ inTags "mo" [] "&#x2209;"
  register [Math] "gets" $ inTags "mo" [] "&#x2190;"
  register [Math] "leftarrow" $ inTags "mo" [] "&#x2190;"
  register [Math] "to" $ inTags "mo" [] "&#x2192;"
  register [Math] "rightarrow" $ inTags "mo" [] "&#x2192;"
  register [Math] "leftrightarrow" $ inTags "mo" [] "&#x2194;"
  register [Math] "uparrow" $ inTags "mo" [] "&#x2191;"
  register [Math] "downarrow" $ inTags "mo" [] "&#x2193;"
  register [Math] "updownarrow" $ inTags "mo" [] "&#x2195;"
  register [Math] "Leftarrow" $ inTags "mo" [] "&#x21D0;"
  register [Math] "Rightarrow" $ inTags "mo" [] "&#x21D2;"
  register [Math] "Leftrightarrow" $ inTags "mo" [] "&#x21D4;"
  register [Math] "iff" $ inTags "mo" [] "&#x21D4;"
  register [Math] "Uparrow" $ inTags "mo" [] "&#x21D1;"
  register [Math] "Downarrow" $ inTags "mo" [] "&#x21D3;"
  register [Math] "Updownarrow" $ inTags "mo" [] "&#x21D5;"
  register [Math] "mapsto" $ inTags "mo" [] "&#x21A6;"
  register [Math] "longleftarrow" $ inTags "mo" [] "&#x2190;"
  register [Math] "longrightarrow" $ inTags "mo" [] "&#x2192;"
  register [Math] "longleftrightarrow" $ inTags "mo" [] "&#x2194;"
  register [Math] "Longleftarrow" $ inTags "mo" [] "&#x21D0;"
  register [Math] "Longrightarrow" $ inTags "mo" [] "&#x21D2;"
  register [Math] "Longleftrightarrow" $ inTags "mo" [] "&#x21D4;"
  register [Math] "longmapsto" $ inTags "mo" [] "&#x21A6;"
  register [Math] "sum" $ inTags "mo" [] "&#x2211;"
  register [Math] "prod" $ inTags "mo" [] "&#x220F;"
  register [Math] "bigcap" $ inTags "mo" [] "&#x22C2;"
  register [Math] "bigcup" $ inTags "mo" [] "&#x22C3;"
  register [Math] "bigwedge" $ inTags "mo" [] "&#x22C0;"
  register [Math] "bigvee" $ inTags "mo" [] "&#x22C1;"
  register [Math] "bigsqcap" $ inTags "mo" [] "&#x2A05;"
  register [Math] "bigsqcup" $ inTags "mo" [] "&#x2A06;"
  register [Math] "coprod" $ inTags "mo" [] "&#x2210;"
  register [Math] "bigoplus" $ inTags "mo" [] "&#x2A01;"
  register [Math] "bigotimes" $ inTags "mo" [] "&#x2A02;"
  register [Math] "bigodot" $ inTags "mo" [] "&#x2A00;"
  register [Math] "biguplus" $ inTags "mo" [] "&#x2A04;"
  register [Math] "int" $ inTags "mo" [] "&#x222B;"
  register [Math] "iint" $ inTags "mo" [] "&#x222C;"
  register [Math] "iiint" $ inTags "mo" [] "&#x222D;"
  register [Math] "oint" $ inTags "mo" [] "&#x222E;"
  register [Math] "prime" $ inTags "mo" [] "&#x2032;"
  register [Math] "dots" $ inTags "mo" [] "&#x2026;"
  register [Math] "ldots" $ inTags "mo" [] "&#x2026;"
  register [Math] "cdots" $ inTags "mo" [] "&#x22EF;"
  register [Math] "vdots" $ inTags "mo" [] "&#x22EE;"
  register [Math] "ddots" $ inTags "mo" [] "&#x22F1;"
  register [Math] "forall" $ inTags "mo" [] "&#x2200;"
  register [Math] "exists" $ inTags "mo" [] "&#x2203;"
  register [Math] "Re" $ inTags "mo" [] "&#x211C;"
  register [Math] "Im" $ inTags "mo" [] "&#x2111;"
  register [Math] "aleph" $ inTags "mo" [] "&#x2135;"
  register [Math] "hbar" $ inTags "mo" [] "&#x210F;"
  register [Math] "ell" $ inTags "mo" [] "&#x2113;"
  register [Math] "wp" $ inTags "mo" [] "&#x2118;"
  register [Math] "emptyset" $ inTags "mo" [] "&#x2205;"
  register [Math] "infty" $ inTags "mo" [] "&#x221E;"
  register [Math] "partial" $ inTags "mo" [] "&#x2202;"
  register [Math] "nabla" $ inTags "mo" [] "&#x2207;"
  register [Math] "triangle" $ inTags "mo" [] "&#x25B3;"
  register [Math] "therefore" $ inTags "mo" [] "&#x2234;"
  register [Math] "angle" $ inTags "mo" [] "&#x2220;"
  register [Math] "diamond" $ inTags "mo" [] "&#x22C4;"
  register [Math] "Diamond" $ inTags "mo" [] "&#x25C7;"
  register [Math] "lozenge" $ inTags "mo" [] "&#x25CA;"
  register [Math] "neg" $ inTags "mo" [] "&#x00AC;"
  register [Math] "lnot" $ inTags "mo" [] "&#x00AC;"
  register [Math] "bot" $ inTags "mo" [] "&#x22A5;"
  register [Math] "top" $ inTags "mo" [] "&#x22A4;"
  register [Math] "square" $ inTags "mo" [] "&#x25AB;"
  register [Math] "Box" $ inTags "mo" [] "&#x25A1;"
  register [Math] "wr" $ inTags "mo" [] "&#x2240;"
  register [Math] "arccos" $ inTags "mi" [] "arccos"
  register [Math] "arcsin" $ inTags "mi" [] "arcsin"
  register [Math] "arctan" $ inTags "mi" [] "arctan"
  register [Math] "arg" $ inTags "mi" [] "arg"
  register [Math] "cos" $ inTags "mi" [] "cos"
  register [Math] "cosh" $ inTags "mi" [] "cosh"
  register [Math] "cot" $ inTags "mi" [] "cot"
  register [Math] "coth" $ inTags "mi" [] "coth"
  register [Math] "csc" $ inTags "mi" [] "csc"
  register [Math] "deg" $ inTags "mi" [] "deg"
  register [Math] "det" $ inTags "mi" [] "det"
  register [Math] "dim" $ inTags "mi" [] "dim"
  register [Math] "exp" $ inTags "mi" [] "exp"
  register [Math] "gcd" $ inTags "mi" [] "gcd"
  register [Math] "hom" $ inTags "mi" [] "hom"
  register [Math] "inf" $ inTags "mi" [] "inf"
  register [Math] "ker" $ inTags "mi" [] "ker"
  register [Math] "lg" $ inTags "mi" [] "lg"
  register [Math] "lim" $ inTags "mi" [] "lim"
  register [Math] "liminf" $ inTags "mi" [] "liminf"
  register [Math] "limsup" $ inTags "mi" [] "limsup"
  register [Math] "ln" $ inTags "mi" [] "ln"
  register [Math] "log" $ inTags "mi" [] "log"
  register [Math] "max" $ inTags "mi" [] "max"
  register [Math] "min" $ inTags "mi" [] "min"
  register [Math] "Pr" $ inTags "mi" [] "Pr"
  register [Math] "sec" $ inTags "mi" [] "sec"
  register [Math] "sin" $ inTags "mi" [] "sin"
  register [Math] "sinh" $ inTags "mi" [] "sinh"
  register [Math] "sup" $ inTags "mi" [] "sup"
  register [Math] "tan" $ inTags "mi" [] "tan"
  register [Math] "tanh" $ inTags "mi" [] "tanh"
  register [Math] "setminus" $ inTags "mo" [] "\\"
  register [Math] "!" $ inTags "mspace" [("width","-0.167em")] mempty
  register [Math] "," $ inTags "mspace" [("width","0.167em")] mempty
  register [Math] "," $ inTags "mspace" [("width","0.167em")] mempty
  register [Math] ">" $ inTags "mspace" [("width","0.222em")] mempty
  register [Math] ":" $ inTags "mspace" [("width","0.222em")] mempty
  register [Math] ";" $ inTags "mspace" [("width","0.278em")] mempty
  register [Math] "quad" $ inTags "mspace" [("width","1em")] mempty
  register [Math] "qquad" $ inTags "mspace" [("width","2em")] mempty
  register [Math] "frac" $ \(MathDoc x) (MathDoc y) ->
    inTags "mfrac" [] (x +++ y)
  register [Math] "tfrac" $ \(MathDoc x) (MathDoc y) ->
    inTags "mstyle" [("displaystyle","false")] $ inTags "mfrac" [] (x +++ y)
  register [Math] "dfrac" $ \(MathDoc x) (MathDoc y) ->
    inTags "mstyle" [("displaystyle","true")] $ inTags "mfrac" [] (x +++ y)
  register [Math] "stackrel" $ \(MathDoc x) (MathDoc y) ->
    inTags "mover" [] (x +++ y)
  register [Math] "overset" $ \(MathDoc x) (MathDoc y) ->
    inTags "mover" [] (x +++ y)
  register [Math] "underset" $ \(MathDoc x) (MathDoc y) ->
    inTags "munder" [] (x +++ y)
  register [Math] "binom" $ \(MathDoc x) (MathDoc y) ->
    inTags "mfenced" [] $ inTags "mfrac" [("linethickness","0")] (x +++ y)

writer :: MathWriter
writer = MathWriter{
   mathFormat = "mathml"
 , displayMath = \d ->
     inTags "math" [("display","block"), ("xmlns",xmlns)] $ mrow d
 , inlineMath = \d ->
     inTags "math" [("display","inline"), ("xmlns",xmlns)] $ mrow d
 , grouped = mrow
 , variable = inTags "mi" [] . rawc
 , number = inTags "mn" [] . raws
 , operator = showOp
 , whitespace = const mempty
 }

mrow :: Doc -> Doc
mrow = inTags "mrow" []

mover :: Doc -> Doc
mover = inTags "mover" []

xmlns :: String
xmlns = "http://www.w3.org/1998/Math/MathML"

root :: Maybe MathDoc -> MathDoc -> Doc
root Nothing (MathDoc y)  =
  inTags "msqrt" [] $ inTags "mn" [] y
root (Just (MathDoc x)) (MathDoc y) =
  inTags "mroot" [] $ inTags "mn" [] y +++ inTags "mn" [] x

showOp :: String -> Doc
showOp s = inTags "mo" []
         $ case s of
              "~"    -> inTags "mspace" [("width","0.333em")] mempty
              "'"    -> raws "\x02B9"
              "''"   -> raws "\x02BA"
              "'''"  -> raws "\x2034"
              "''''" -> raws "\x2057"
              _      -> raws s

asText :: String -> Doc -> Doc
asText variant = inTags "mtext" [("mathvariant",variant)]

enclosure :: HeX Doc
enclosure = try $ do
  modif <- try (string "\\left" >> spaces >> return "left")
        <|> try (string "\\right" >> spaces >> return "right")
        <|> scaler
        <|> return ""
  enc <- basicEnclosure
      <|> if (modif == "left" || modif == "right")
             then try (char '.' >> return '\xFEFF')
             else fail "expecting enclosure"
  case modif of
       ""       -> return $ inTags "mo" [] (rawc enc)
       "left"   -> tilRight enc
               <|> return (inTags "mo" [("stretchy","true")] (rawc enc))
       "right"  -> return $ inTags "mo" [("stretchy","true")] (rawc enc)
       scale    -> return $ inTags "mo"
                     [("stretchy","true"),("minsize",scale),("maxsize",scale)]
                     (rawc enc)

basicEnclosure :: HeX Char
basicEnclosure = (oneOf "[]()" <|> (char '|' >> return '\x2223'))
              <|> try (do char '\\'
                          cmd <- many1 letter <|> count 1 anyChar
                          case M.lookup cmd enclosures of
                                Just x   -> return x
                                Nothing  -> fail "not an enclosure")

enclosures :: M.Map String Char
enclosures = M.fromList
  [ ("{", '{')
  , ("}", '}')
  , ("lbrack", '[')
  , ("lbrace", '{')
  , ("rbrack", ']')
  , ("rbrace", '}')
  , ("llbracket", '\x27E6')
  , ("rrbracket", '\x27E7')
  , ("langle", '\x27E8')
  , ("rangle", '\x27E9')
  , ("lfloor", '\x230A')
  , ("rfloor", '\x230B')
  , ("lceil", '\x2308')
  , ("rceil", '\x2309')
  , ("|", '\x2225')
  , ("|", '\x2225')
  , ("lvert", '\x7C')
  , ("rvert", '\x7C')
  , ("vert", '\x7C')
  , ("lVert", '\x2225')
  , ("rVert", '\x2225')
  , ("Vert", '\x2016')
  , ("ulcorner", '\x231C')
  , ("urcorner", '\x231D')
  ]

-- We want stuff between \left( and \right) to be in an mrow,
-- so that the scaling is based just on this unit, and not the
-- whole containing formula.
tilRight :: Char -> HeX Doc
tilRight start = try $ do
  contents <- manyTill math
               (try $ string "\\right" >> lookAhead basicEnclosure)
  end <- basicEnclosure
  return $ mrow $ inTags "mo" [("stretchy","true")] (rawc start) +++
    mconcat contents +++ inTags "mo" [("stretchy","true")] (rawc end)

scaler :: HeX String
scaler = try $ do
  char '\\'
  cmd <- many1 letter
  case M.lookup cmd scalers of
       Just  r -> return r
       Nothing -> unexpected $ '\\' : cmd

scalers :: M.Map String String
scalers = M.fromList
          [ ("bigg", "2.2")
          , ("Bigg", "2.9")
          , ("big", "1.2")
          , ("Big", "1.6")
          , ("biggr", "2.2")
          , ("Biggr", "2.9")
          , ("bigr", "1.2")
          , ("Bigr", "1.6")
          , ("biggl", "2.2")
          , ("Biggl", "2.9")
          , ("bigl", "1.2")
          , ("Bigl", "1.6")
          ]

-- 'wraps' a parser in a check for super/subscript/limits
subsup :: [HeX Doc] -> HeX Doc
subsup parsers = do
  res <- choice parsers
  resStr <- case res of
                 Doc x -> return $ toString $ toLazyByteString x
                 Fut _ -> error "Unexpected Fut in math mode"
  limits <- Just <$> limitsIndicator <|> return Nothing
  sub <- Just <$> subscript <|> return Nothing
  sup <- Just <$> superscript <|> return Nothing
  (displaymath :: Bool) <- getVar "displaymath"
  let convertibleSymbols = ['\x2211','\x220F','\x22C2',
        '\x22C3','\x22C0','\x22C1','\x2A05','\x2A06',
        '\x2210','\x2A01','\x2A02','\x2A00','\x2A04',
        '-', '*', '>', '<', '=', ':', '\x2223', '\x2225',
        '\x2216', '/', '\\', '\x00D7', '\x00B1', '\x2213',
        '\x22B2', '\x22B3', '\x22C5', '\x22C6', '\x002A', '\x00D7',
        '\x00F7', '\x2218', '\x2022', '\x2295', '\x2296', '\x2297',
        '\x25CB', '\x2298', '\x2299', '\x2227', '\x2227', '\x2228',
        '\x2228', '\x2229', '\x222A', '\x2293', '\x2294', '\x228E',
        '\x2210', '\x25B3', '\x25BD', '\x2020', '\x2020', '\x2021',
        '\x2021', '\x22B2', '\x22B3', '\x22B4', '\x22B5', '<',
        '>', '\x2260', '\x2260', '\x2264', '\x2264', '\x2264',
        '\x2265', '\x2265', '\x2265', '\x2261', '\x226A', '\x226B',
        '\x2250', '\x227A', '\x227B', '\x227C', '\x227D', '\x2282',
        '\x2283', '\x2286', '\x2287', '\x228F', '\x2290', '\x2291',
        '\x2292', '\x223C', '\x2243', '\x2248', '\x2245', '\x22C8',
        '\x22C8', '\x2208', '\x220B', '\x220B', '\x221D', '\x22A2',
        '\x22A3', '\x22A8', '\x22A5', '\x2323', '\x2322', '\x224D',
        '\x2209', '\x2190', '\x2190', '\x2192', '\x2192', '\x2194',
        '\x2191', '\x2193', '\x2195', '\x21D0', '\x21D2', '\x21D4',
        '\x21D4', '\x21D1', '\x21D3', '\x21D5', '\x21A6', '\x2190',
        '\x2192', '\x2194', '\x21D0', '\x21D2', '\x21D4', '\x21A6' ]
  let isConvertible = displaymath &&
                    (   "<munder" `isPrefixOf` resStr
                     || "<mover"  `isPrefixOf` resStr
                     || "<mi>lim" `isPrefixOf` resStr
                     || "<mi>inf" `isPrefixOf` resStr
                     || "<mi>sup" `isPrefixOf` resStr
                     || ( "<mo>" `isPrefixOf` resStr &&
                          not (null $ drop 4 resStr) &&
                          resStr !! 4 `elem` convertibleSymbols )
                    )
  return $
    case (sub, sup, limits) of
       (Nothing, Nothing, _)         -> res
       (Just x, Nothing, Nothing)
                    | isConvertible  -> inTags "munder" [] $ res +++ x
       (Just x, Nothing, Just True)  -> inTags "munder" [] $ res +++ x
       (Just x, Nothing, _)          -> inTags "msub" [] $ res +++ x
       (Nothing, Just y, Nothing)
                    | isConvertible  -> inTags "mover" [] $ res +++ y
       (Nothing, Just y, Just True)  -> inTags "mover" [] $ res +++ y
       (Nothing, Just y, _)          -> inTags "msup" [] $ res +++ y
       (Just x, Just y, Nothing)
                    | isConvertible  -> inTags "munderover" [] $ res +++ x +++ y
       (Just x, Just y, Just True)   -> inTags "munderover" [] $ res +++ x +++ y
       (Just x, Just y, _)           -> inTags "msubsup" [] $ res +++ x +++ y

limitsIndicator :: HeX Bool
limitsIndicator = try $ do
  char '\\'
  no <- option False (string "no" >> return True)
  string "limits"
  spaces
  return $ not $ no

subscript :: HeX Doc
subscript = try $ char '_' >> math

superscript :: HeX Doc
superscript = try $ char '^' >> math

{-
  -- limits <- limitsIndicator
  -- subSup limits a <|> superOrSubscripted limits a <|> return a

expr :: GenParser Char st Exp
expr = do
  a <- expr1
  limits <- limitsIndicator
  subSup limits a <|> superOrSubscripted limits a <|> return a

limitsIndicator :: GenParser Char st (Maybe Bool)
limitsIndicator =
   try (symbol "\\limits" >> return (Just True))
  <|> try (symbol "\\nolimits" >> return (Just False))
  <|> return Nothing

subSup :: Maybe Bool -> Exp -> GenParser Char st Exp
subSup limits a = try $ do
  char '_'
  b <- expr1
  char '^'
  c <- expr
  return $ case limits of
            Just True  -> EUnderover a b c
            Nothing | isConvertible a -> EDownup a b c
            _          -> ESubsup a b c

superOrSubscripted :: Maybe Bool -> Exp -> GenParser Char st Exp
superOrSubscripted limits a = try $ do
  c <- oneOf "^_"
  b <- expr
  case c of
       '^' -> return $ case limits of
                        Just True  -> EOver a b
                        Nothing | isConvertible a -> EUp a b
                        _          -> ESuper a b
       '_' -> return $ case limits of
                        Just True  -> EUnder a b
                        Nothing | isConvertible a -> EDown a b
                        _          -> ESub a b
       _   -> pzero

isConvertible :: Exp -> Bool
isConvertible (EMathOperator x) = x `elem` convertibleOps
  where convertibleOps = ["lim","liminf","limsup","inf","sup"]
isConvertible (ESymbol Rel _) = True
isConvertible (ESymbol Bin _) = True
isConvertible (EUnder _ _)    = True
isConvertible (EOver _ _)     = True
isConvertible (EUnderover _ _ _) = True
isConvertible (ESymbol Op x) = x `elem` convertibleSyms
  where convertibleSyms = ["\x2211","\x220F","\x22C2",
           "\x22C3","\x22C0","\x22C1","\x2A05","\x2A06",
           "\x2210","\x2A01","\x2A02","\x2A00","\x2A04"]
isConvertible _ = False

symbols :: M.Map String Exp
symbols = M.fromList [
             ("+", ESymbol Bin "+")
           , ("-", ESymbol Bin "-")
           , ("*", ESymbol Bin "*")
           , (",", ESymbol Pun ",")
           , (".", ESymbol Pun ".")
           , (";", ESymbol Pun ";")
           , (":", ESymbol Pun ":")
           , ("?", ESymbol Pun "?")
           , (">", ESymbol Rel ">")
           , ("<", ESymbol Rel "<")
           , ("!", ESymbol Ord "!")
           , ("'", ESymbol Ord "\x02B9")
           , ("''", ESymbol Ord "\x02BA")
           , ("'''", ESymbol Ord "\x2034")
           , ("''''", ESymbol Ord "\x2057")
           , ("=", ESymbol Rel "=")
           , (":=", ESymbol Rel ":=")
           , ("\\mid", ESymbol Bin "\x2223")
           , ("\\parallel", ESymbol Rel "\x2225")
           , ("\\backslash", ESymbol Bin "\x2216")
           , ("/", ESymbol Bin "/")
           , ("\\setminus",	ESymbol Bin "\\")
           , ("\\times", ESymbol Bin "\x00D7")
           , ("\\alpha", ESymbol Ord "\x03B1")
           , ("\\beta", ESymbol Ord "\x03B2")
           , ("\\chi", ESymbol Ord "\x03C7")
           , ("\\delta", ESymbol Ord "\x03B4")
           , ("\\Delta", ESymbol Op "\x0394")
           , ("\\epsilon", ESymbol Ord "\x03B5")
           , ("\\varepsilon", ESymbol Ord "\x025B")
           , ("\\eta", ESymbol Ord "\x03B7")
           , ("\\gamma", ESymbol Ord "\x03B3")
           , ("\\Gamma", ESymbol Op "\x0393") 
           , ("\\iota", ESymbol Ord "\x03B9")
           , ("\\kappa", ESymbol Ord "\x03BA")
           , ("\\lambda", ESymbol Ord "\x03BB")
           , ("\\Lambda", ESymbol Op "\x039B") 
           , ("\\mu", ESymbol Ord "\x03BC")
           , ("\\nu", ESymbol Ord "\x03BD")
           , ("\\omega", ESymbol Ord "\x03C9")
           , ("\\Omega", ESymbol Op "\x03A9")
           , ("\\phi", ESymbol Ord "\x03C6")
           , ("\\varphi", ESymbol Ord "\x03D5")
           , ("\\Phi", ESymbol Op "\x03A6") 
           , ("\\pi", ESymbol Ord "\x03C0")
           , ("\\Pi", ESymbol Op "\x03A0") 
           , ("\\psi", ESymbol Ord "\x03C8")
           , ("\\Psi", ESymbol Ord "\x03A8")
           , ("\\rho", ESymbol Ord "\x03C1")
           , ("\\sigma", ESymbol Ord "\x03C3")
           , ("\\Sigma", ESymbol Op "\x03A3") 
           , ("\\tau", ESymbol Ord "\x03C4")
           , ("\\theta", ESymbol Ord "\x03B8")
           , ("\\vartheta", ESymbol Ord "\x03D1")
           , ("\\Theta", ESymbol Op "\x0398") 
           , ("\\upsilon", ESymbol Ord "\x03C5")
           , ("\\xi", ESymbol Ord "\x03BE")
           , ("\\Xi", ESymbol Op "\x039E") 
           , ("\\zeta", ESymbol Ord "\x03B6")
           , ("\\frac12", ESymbol Ord "\x00BD")
           , ("\\frac14", ESymbol Ord "\x00BC")
           , ("\\frac34", ESymbol Ord "\x00BE")
           , ("\\frac13", ESymbol Ord "\x2153")
           , ("\\frac23", ESymbol Ord "\x2154")
           , ("\\frac15", ESymbol Ord "\x2155")
           , ("\\frac25", ESymbol Ord "\x2156")
           , ("\\frac35", ESymbol Ord "\x2157")
           , ("\\frac45", ESymbol Ord "\x2158")
           , ("\\frac16", ESymbol Ord "\x2159")
           , ("\\frac56", ESymbol Ord "\x215A")
           , ("\\frac18", ESymbol Ord "\x215B")
           , ("\\frac38", ESymbol Ord "\x215C")
           , ("\\frac58", ESymbol Ord "\x215D")
           , ("\\frac78", ESymbol Ord "\x215E")
           , ("\\pm", ESymbol Bin "\x00B1")
           , ("\\mp", ESymbol Bin "\x2213")
           , ("\\triangleleft", ESymbol Bin "\x22B2")
           , ("\\triangleright", ESymbol Bin "\x22B3")
           , ("\\cdot", ESymbol Bin "\x22C5")
           , ("\\star", ESymbol Bin "\x22C6")
           , ("\\ast", ESymbol Bin "\x002A")
           , ("\\times", ESymbol Bin "\x00D7")
           , ("\\div", ESymbol Bin "\x00F7")
           , ("\\circ", ESymbol Bin "\x2218")
           , ("\\bullet", ESymbol Bin "\x2022")
           , ("\\oplus", ESymbol Bin "\x2295")
           , ("\\ominus", ESymbol Bin "\x2296")
           , ("\\otimes", ESymbol Bin "\x2297")
           , ("\\bigcirc", ESymbol Bin "\x25CB")
           , ("\\oslash", ESymbol Bin "\x2298")
           , ("\\odot", ESymbol Bin "\x2299")
           , ("\\land", ESymbol Bin "\x2227")
           , ("\\wedge", ESymbol Bin "\x2227")
           , ("\\lor", ESymbol Bin "\x2228")
           , ("\\vee", ESymbol Bin "\x2228")
           , ("\\cap", ESymbol Bin "\x2229")
           , ("\\cup", ESymbol Bin "\x222A")
           , ("\\sqcap", ESymbol Bin "\x2293")
           , ("\\sqcup", ESymbol Bin "\x2294")
           , ("\\uplus", ESymbol Bin "\x228E")
           , ("\\amalg", ESymbol Bin "\x2210")
           , ("\\bigtriangleup", ESymbol Bin "\x25B3")
           , ("\\bigtriangledown", ESymbol Bin "\x25BD")
           , ("\\dag", ESymbol Bin "\x2020")
           , ("\\dagger", ESymbol Bin "\x2020")
           , ("\\ddag", ESymbol Bin "\x2021")
           , ("\\ddagger", ESymbol Bin "\x2021")
           , ("\\lhd", ESymbol Bin "\x22B2")
           , ("\\rhd", ESymbol Bin "\x22B3")
           , ("\\unlhd", ESymbol Bin "\x22B4")
           , ("\\unrhd", ESymbol Bin "\x22B5")
           , ("\\lt", ESymbol Rel "<")
           , ("\\gt", ESymbol Rel ">")
           , ("\\ne", ESymbol Rel "\x2260")
           , ("\\neq", ESymbol Rel "\x2260")
           , ("\\le", ESymbol Rel "\x2264")
           , ("\\leq", ESymbol Rel "\x2264")
           , ("\\leqslant", ESymbol Rel "\x2264")
           , ("\\ge", ESymbol Rel "\x2265")
           , ("\\geq", ESymbol Rel "\x2265")
           , ("\\geqslant", ESymbol Rel "\x2265")
           , ("\\equiv", ESymbol Rel "\x2261")
           , ("\\ll", ESymbol Rel "\x226A")
           , ("\\gg", ESymbol Rel "\x226B")
           , ("\\doteq", ESymbol Rel "\x2250")
           , ("\\prec", ESymbol Rel "\x227A")
           , ("\\succ", ESymbol Rel "\x227B")
           , ("\\preceq", ESymbol Rel "\x227C")
           , ("\\succeq", ESymbol Rel "\x227D")
           , ("\\subset", ESymbol Rel "\x2282")
           , ("\\supset", ESymbol Rel "\x2283")
           , ("\\subseteq", ESymbol Rel "\x2286")
           , ("\\supseteq", ESymbol Rel "\x2287")
           , ("\\sqsubset", ESymbol Rel "\x228F")
           , ("\\sqsupset", ESymbol Rel "\x2290")
           , ("\\sqsubseteq", ESymbol Rel "\x2291")
           , ("\\sqsupseteq", ESymbol Rel "\x2292")
           , ("\\sim", ESymbol Rel "\x223C")
           , ("\\simeq", ESymbol Rel "\x2243")
           , ("\\approx", ESymbol Rel "\x2248")
           , ("\\cong", ESymbol Rel "\x2245")
           , ("\\Join", ESymbol Rel "\x22C8")
           , ("\\bowtie", ESymbol Rel "\x22C8")
           , ("\\in", ESymbol Rel "\x2208")
           , ("\\ni", ESymbol Rel "\x220B")
           , ("\\owns", ESymbol Rel "\x220B")
           , ("\\propto", ESymbol Rel "\x221D")
           , ("\\vdash", ESymbol Rel "\x22A2")
           , ("\\dashv", ESymbol Rel "\x22A3")
           , ("\\models", ESymbol Rel "\x22A8")
           , ("\\perp", ESymbol Rel "\x22A5")
           , ("\\smile", ESymbol Rel "\x2323")
           , ("\\frown", ESymbol Rel "\x2322")
           , ("\\asymp", ESymbol Rel "\x224D")
           , ("\\notin", ESymbol Rel "\x2209")
           , ("\\gets", ESymbol Rel "\x2190")
           , ("\\leftarrow", ESymbol Rel "\x2190")
           , ("\\to", ESymbol Rel "\x2192")
           , ("\\rightarrow", ESymbol Rel "\x2192")
           , ("\\leftrightarrow", ESymbol Rel "\x2194")
           , ("\\uparrow", ESymbol Rel "\x2191")
           , ("\\downarrow", ESymbol Rel "\x2193")
           , ("\\updownarrow", ESymbol Rel "\x2195")
           , ("\\Leftarrow", ESymbol Rel "\x21D0")
           , ("\\Rightarrow", ESymbol Rel "\x21D2")
           , ("\\Leftrightarrow", ESymbol Rel "\x21D4")
           , ("\\iff", ESymbol Rel "\x21D4")
           , ("\\Uparrow", ESymbol Rel "\x21D1")
           , ("\\Downarrow", ESymbol Rel "\x21D3")
           , ("\\Updownarrow", ESymbol Rel "\x21D5")
           , ("\\mapsto", ESymbol Rel "\x21A6")
           , ("\\longleftarrow", ESymbol Rel "\x2190")
           , ("\\longrightarrow", ESymbol Rel "\x2192")
           , ("\\longleftrightarrow", ESymbol Rel "\x2194")
           , ("\\Longleftarrow", ESymbol Rel "\x21D0")
           , ("\\Longrightarrow", ESymbol Rel "\x21D2")
           , ("\\Longleftrightarrow", ESymbol Rel "\x21D4")
           , ("\\longmapsto", ESymbol Rel "\x21A6")
           , ("\\sum", ESymbol Op "\x2211")
           , ("\\prod", ESymbol Op "\x220F")
           , ("\\bigcap", ESymbol Op "\x22C2")
           , ("\\bigcup", ESymbol Op "\x22C3")
           , ("\\bigwedge", ESymbol Op "\x22C0")
           , ("\\bigvee", ESymbol Op "\x22C1")
           , ("\\bigsqcap", ESymbol Op "\x2A05")
           , ("\\bigsqcup", ESymbol Op "\x2A06")
           , ("\\coprod", ESymbol Op "\x2210")
           , ("\\bigoplus", ESymbol Op "\x2A01")
           , ("\\bigotimes", ESymbol Op "\x2A02")
           , ("\\bigodot", ESymbol Op "\x2A00")
           , ("\\biguplus", ESymbol Op "\x2A04")
           , ("\\int", ESymbol Op "\x222B")
           , ("\\iint", ESymbol Op "\x222C")
           , ("\\iiint", ESymbol Op "\x222D")
           , ("\\oint", ESymbol Op "\x222E")
           , ("\\prime", ESymbol Ord "\x2032")
           , ("\\dots", ESymbol Ord "\x2026")
           , ("\\ldots", ESymbol Ord "\x2026")
           , ("\\cdots", ESymbol Ord "\x22EF")
           , ("\\vdots", ESymbol Ord "\x22EE")
           , ("\\ddots", ESymbol Ord "\x22F1")
           , ("\\forall", ESymbol Op "\x2200")
           , ("\\exists", ESymbol Op "\x2203")
           , ("\\Re", ESymbol Ord "\x211C")
           , ("\\Im", ESymbol Ord "\x2111")
           , ("\\aleph", ESymbol Ord "\x2135")
           , ("\\hbar", ESymbol Ord "\x210F")
           , ("\\ell", ESymbol Ord "\x2113")
           , ("\\wp", ESymbol Ord "\x2118")
           , ("\\emptyset", ESymbol Ord "\x2205")
           , ("\\infty", ESymbol Ord "\x221E")
           , ("\\partial", ESymbol Ord "\x2202")
           , ("\\nabla", ESymbol Ord "\x2207")
           , ("\\triangle", ESymbol Ord "\x25B3")
           , ("\\therefore", ESymbol Pun "\x2234")
           , ("\\angle", ESymbol Ord "\x2220")
           , ("\\diamond", ESymbol Op "\x22C4")
           , ("\\Diamond", ESymbol Op "\x25C7")
           , ("\\lozenge", ESymbol Op "\x25CA")
           , ("\\neg", ESymbol Op "\x00AC")
           , ("\\lnot", ESymbol Ord "\x00AC")
           , ("\\bot", ESymbol Ord "\x22A5")
           , ("\\top", ESymbol Ord "\x22A4")
           , ("\\square", ESymbol Ord "\x25AB")
           , ("\\Box", ESymbol Op "\x25A1")
           , ("\\wr", ESymbol Ord "\x2240")
           , ("\\!", ESpace "-0.167em")
           , ("\\,", ESpace "0.167em")
           , ("\\>", ESpace "0.222em")
           , ("\\:", ESpace "0.222em")
           , ("\\;", ESpace "0.278em")
           , ("~", ESpace "0.333em")
           , ("\\quad", ESpace "1em")
           , ("\\qquad", ESpace "2em")
           , ("\\arccos", EMathOperator "arccos")
           , ("\\arcsin", EMathOperator "arcsin")
           , ("\\arctan", EMathOperator "arctan")
           , ("\\arg", EMathOperator "arg")
           , ("\\cos", EMathOperator "cos")
           , ("\\cosh", EMathOperator "cosh")
           , ("\\cot", EMathOperator "cot")
           , ("\\coth", EMathOperator "coth")
           , ("\\csc", EMathOperator "csc")
           , ("\\deg", EMathOperator "deg")
           , ("\\det", EMathOperator "det")
           , ("\\dim", EMathOperator "dim")
           , ("\\exp", EMathOperator "exp")
           , ("\\gcd", EMathOperator "gcd")
           , ("\\hom", EMathOperator "hom")
           , ("\\inf", EMathOperator "inf")
           , ("\\ker", EMathOperator "ker")
           , ("\\lg", EMathOperator "lg")
           , ("\\lim", EMathOperator "lim")
           , ("\\liminf", EMathOperator "liminf")
           , ("\\limsup", EMathOperator "limsup")
           , ("\\ln", EMathOperator "ln")
           , ("\\log", EMathOperator "log")
           , ("\\max", EMathOperator "max")
           , ("\\min", EMathOperator "min")
           , ("\\Pr", EMathOperator "Pr")
           , ("\\sec", EMathOperator "sec")
           , ("\\sin", EMathOperator "sin")
           , ("\\sinh", EMathOperator "sinh")
           , ("\\sup", EMathOperator "sup")
           , ("\\tan", EMathOperator "tan")
           , ("\\tanh", EMathOperator "tanh")
           ] 


-}

