{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Text.HeX.Math.MathML (defaults, arrayEnv) where

import Text.HeX
import Text.HeX.Standard.Xml
import Control.Applicative ((<$>))
import Text.HeX.Math (defaultsFor, arrayLines)
import qualified Data.Map as M
import Data.ByteString.Lazy.UTF8 (toString)
import Blaze.ByteString.Builder
import Data.List (isPrefixOf)

defaults :: HeX ()
defaults = do
  defaultsFor writer
  addParser [Math] enclosure
  updateState $ \st -> st{ hexParsers =
     M.adjust (\x -> subsup x) Math $ hexParsers st }
  newCommand [Math] "textrm" $ asText "normal" <$> inline
  newCommand [Math] "text" $ asText "normal" <$> inline
  newCommand [Math] "mathrm" $ asText "normal" <$> math
  newCommand [Math] "mbox" $ asText "normal" <$> math
  newCommand [Math] "mathit" $ asText "italic" <$> math
  newCommand [Math] "textit" $ asText "italic" <$> inline
  newCommand [Math] "mathtt" $ asText "monospace" <$> math
  newCommand [Math] "texttt" $ asText "monospace" <$> inline
  newCommand [Math] "mathsf" $ asText "sans-serif" <$> math
  newCommand [Math] "mathbb" $ asText "double-struck" <$> math
  newCommand [Math] "mathcal" $ asText "script" <$> math
  newCommand [Math] "mathfrak" $ asText "fraktur" <$> math
  newCommand [Math] "sqrt" root
  newCommand [Math] "surd" root
  newCommand [Math] "acute" $ \(MathDoc d) ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#180;"
  newCommand [Math] "grave" $ \(MathDoc d) ->
    mover $ d +++ inTags "mo" [("accent","true")] "`"
  newCommand [Math] "breve" $ \(MathDoc d) ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#728;"
  newCommand [Math] "check" $ \(MathDoc d) ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#711;"
  newCommand [Math] "dot" $ \(MathDoc d) ->
    mover $ d +++ inTags "mo" [("accent","true")] "."
  newCommand [Math] "ddot" $ \(MathDoc d) ->
    mover $ d +++ inTags "mo" [("accent","true")] ".."
  newCommand [Math] "mathring" $ \(MathDoc d) ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#176;"
  newCommand [Math] "vec" $ \(MathDoc d) ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#8407;"
  newCommand [Math] "overrightarrow" $ \(MathDoc d) ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#8407;"
  newCommand [Math] "overleftarrow" $ \(MathDoc d) ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#8406;"
  newCommand [Math] "hat" $ \(MathDoc d) ->
    mover $ d +++ inTags "mo" [("accent","true")] "^"
  newCommand [Math] "widehat" $ \(MathDoc d) ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#770;"
  newCommand [Math] "tilde" $ \(MathDoc d) ->
    mover $ d +++ inTags "mo" [("accent","true")] "~"
  newCommand [Math] "widetilde" $ \(MathDoc d) ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#732;"
  newCommand [Math] "bar" $ \(MathDoc d) ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#8254;"
  newCommand [Math] "overbrace" $ \(MathDoc d) ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#65079;"
  newCommand [Math] "overbracket" $ \(MathDoc d) ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#9140;"
  newCommand [Math] "overline" $ \(MathDoc d) ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#175;"
  newCommand [Math] "underbrace" $ \(MathDoc d) ->
    inTags "munder" [] $ d +++ inTags "mo" [("accent","true")] "&#65080;"
  newCommand [Math] "underbracket" $ \(MathDoc d) ->
    inTags "munder" [] $ d +++ inTags "mo" [("accent","true")] "&#9141;"
  newCommand [Math] "underline" $ \(MathDoc d) ->
    inTags "munder" [] $ d +++ inTags "mo" [("accent","true")] "&#175;"
  newCommand [Math] "mid" $ inTags "mo" [] "&#x2223;"
  newCommand [Math] "parallel" $ inTags "mo" [] "&#x2225;"
  newCommand [Math] "backslash" $ inTags "mo" [] "&#x2216;"
  newCommand [Math] "times" $ inTags "mo" [] "&#x00D7;"
  newCommand [Math] "alpha" $ inTags "mo" [] "&#x03B1;"
  newCommand [Math] "beta" $ inTags "mo" [] "&#x03B2;"
  newCommand [Math] "chi" $ inTags "mo" [] "&#x03C7;"
  newCommand [Math] "delta" $ inTags "mo" [] "&#x03B4;"
  newCommand [Math] "Delta" $ inTags "mo" [] "&#x0394;"
  newCommand [Math] "epsilon" $ inTags "mo" [] "&#x03B5;"
  newCommand [Math] "varepsilon" $ inTags "mo" [] "&#x025B;"
  newCommand [Math] "eta" $ inTags "mo" [] "&#x03B7;"
  newCommand [Math] "gamma" $ inTags "mo" [] "&#x03B3;"
  newCommand [Math] "Gamma" $ inTags "mo" [] "&#x0393;" 
  newCommand [Math] "iota" $ inTags "mo" [] "&#x03B9;"
  newCommand [Math] "kappa" $ inTags "mo" [] "&#x03BA;"
  newCommand [Math] "lambda" $ inTags "mo" [] "&#x03BB;"
  newCommand [Math] "Lambda" $ inTags "mo" [] "&#x039B;" 
  newCommand [Math] "mu" $ inTags "mo" [] "&#x03BC;"
  newCommand [Math] "nu" $ inTags "mo" [] "&#x03BD;"
  newCommand [Math] "omega" $ inTags "mo" [] "&#x03C9;"
  newCommand [Math] "Omega" $ inTags "mo" [] "&#x03A9;"
  newCommand [Math] "phi" $ inTags "mo" [] "&#x03C6;"
  newCommand [Math] "varphi" $ inTags "mo" [] "&#x03D5;"
  newCommand [Math] "Phi" $ inTags "mo" [] "&#x03A6;" 
  newCommand [Math] "pi" $ inTags "mo" [] "&#x03C0;"
  newCommand [Math] "Pi" $ inTags "mo" [] "&#x03A0;" 
  newCommand [Math] "psi" $ inTags "mo" [] "&#x03C8;"
  newCommand [Math] "Psi" $ inTags "mo" [] "&#x03A8;"
  newCommand [Math] "rho" $ inTags "mo" [] "&#x03C1;"
  newCommand [Math] "sigma" $ inTags "mo" [] "&#x03C3;"
  newCommand [Math] "Sigma" $ inTags "mo" [] "&#x03A3;" 
  newCommand [Math] "tau" $ inTags "mo" [] "&#x03C4;"
  newCommand [Math] "theta" $ inTags "mo" [] "&#x03B8;"
  newCommand [Math] "vartheta" $ inTags "mo" [] "&#x03D1;"
  newCommand [Math] "Theta" $ inTags "mo" [] "&#x0398;" 
  newCommand [Math] "upsilon" $ inTags "mo" [] "&#x03C5;"
  newCommand [Math] "xi" $ inTags "mo" [] "&#x03BE;"
  newCommand [Math] "Xi" $ inTags "mo" [] "&#x039E;" 
  newCommand [Math] "zeta" $ inTags "mo" [] "&#x03B6;"
  newCommand [Math] "frac12" $ inTags "mo" [] "&#x00BD;"
  newCommand [Math] "frac14" $ inTags "mo" [] "&#x00BC;"
  newCommand [Math] "frac34" $ inTags "mo" [] "&#x00BE;"
  newCommand [Math] "frac13" $ inTags "mo" [] "&#x2153;"
  newCommand [Math] "frac23" $ inTags "mo" [] "&#x2154;"
  newCommand [Math] "frac15" $ inTags "mo" [] "&#x2155;"
  newCommand [Math] "frac25" $ inTags "mo" [] "&#x2156;"
  newCommand [Math] "frac35" $ inTags "mo" [] "&#x2157;"
  newCommand [Math] "frac45" $ inTags "mo" [] "&#x2158;"
  newCommand [Math] "frac16" $ inTags "mo" [] "&#x2159;"
  newCommand [Math] "frac56" $ inTags "mo" [] "&#x215A;"
  newCommand [Math] "frac18" $ inTags "mo" [] "&#x215B;"
  newCommand [Math] "frac38" $ inTags "mo" [] "&#x215C;"
  newCommand [Math] "frac58" $ inTags "mo" [] "&#x215D;"
  newCommand [Math] "frac78" $ inTags "mo" [] "&#x215E;"
  newCommand [Math] "pm" $ inTags "mo" [] "&#x00B1;"
  newCommand [Math] "mp" $ inTags "mo" [] "&#x2213;"
  newCommand [Math] "triangleleft" $ inTags "mo" [] "&#x22B2;"
  newCommand [Math] "triangleright" $ inTags "mo" [] "&#x22B3;"
  newCommand [Math] "cdot" $ inTags "mo" [] "&#x22C5;"
  newCommand [Math] "star" $ inTags "mo" [] "&#x22C6;"
  newCommand [Math] "ast" $ inTags "mo" [] "&#x002A;"
  newCommand [Math] "times" $ inTags "mo" [] "&#x00D7;"
  newCommand [Math] "div" $ inTags "mo" [] "&#x00F7;"
  newCommand [Math] "circ" $ inTags "mo" [] "&#x2218;"
  newCommand [Math] "bullet" $ inTags "mo" [] "&#x2022;"
  newCommand [Math] "oplus" $ inTags "mo" [] "&#x2295;"
  newCommand [Math] "ominus" $ inTags "mo" [] "&#x2296;"
  newCommand [Math] "otimes" $ inTags "mo" [] "&#x2297;"
  newCommand [Math] "bigcirc" $ inTags "mo" [] "&#x25CB;"
  newCommand [Math] "oslash" $ inTags "mo" [] "&#x2298;"
  newCommand [Math] "odot" $ inTags "mo" [] "&#x2299;"
  newCommand [Math] "land" $ inTags "mo" [] "&#x2227;"
  newCommand [Math] "wedge" $ inTags "mo" [] "&#x2227;"
  newCommand [Math] "lor" $ inTags "mo" [] "&#x2228;"
  newCommand [Math] "vee" $ inTags "mo" [] "&#x2228;"
  newCommand [Math] "cap" $ inTags "mo" [] "&#x2229;"
  newCommand [Math] "cup" $ inTags "mo" [] "&#x222A;"
  newCommand [Math] "sqcap" $ inTags "mo" [] "&#x2293;"
  newCommand [Math] "sqcup" $ inTags "mo" [] "&#x2294;"
  newCommand [Math] "uplus" $ inTags "mo" [] "&#x228E;"
  newCommand [Math] "amalg" $ inTags "mo" [] "&#x2210;"
  newCommand [Math] "bigtriangleup" $ inTags "mo" [] "&#x25B3;"
  newCommand [Math] "bigtriangledown" $ inTags "mo" [] "&#x25BD;"
  newCommand [Math] "dag" $ inTags "mo" [] "&#x2020;"
  newCommand [Math] "dagger" $ inTags "mo" [] "&#x2020;"
  newCommand [Math] "ddag" $ inTags "mo" [] "&#x2021;"
  newCommand [Math] "ddagger" $ inTags "mo" [] "&#x2021;"
  newCommand [Math] "lhd" $ inTags "mo" [] "&#x22B2;"
  newCommand [Math] "rhd" $ inTags "mo" [] "&#x22B3;"
  newCommand [Math] "unlhd" $ inTags "mo" [] "&#x22B4;"
  newCommand [Math] "unrhd" $ inTags "mo" [] "&#x22B5;"
  newCommand [Math] "lt" $ inTags "mo" [] "<"
  newCommand [Math] "gt" $ inTags "mo" [] ">"
  newCommand [Math] "ne" $ inTags "mo" [] "&#x2260;"
  newCommand [Math] "neq" $ inTags "mo" [] "&#x2260;"
  newCommand [Math] "le" $ inTags "mo" [] "&#x2264;"
  newCommand [Math] "leq" $ inTags "mo" [] "&#x2264;"
  newCommand [Math] "leqslant" $ inTags "mo" [] "&#x2264;"
  newCommand [Math] "ge" $ inTags "mo" [] "&#x2265;"
  newCommand [Math] "geq" $ inTags "mo" [] "&#x2265;"
  newCommand [Math] "geqslant" $ inTags "mo" [] "&#x2265;"
  newCommand [Math] "equiv" $ inTags "mo" [] "&#x2261;"
  newCommand [Math] "ll" $ inTags "mo" [] "&#x226A;"
  newCommand [Math] "gg" $ inTags "mo" [] "&#x226B;"
  newCommand [Math] "doteq" $ inTags "mo" [] "&#x2250;"
  newCommand [Math] "prec" $ inTags "mo" [] "&#x227A;"
  newCommand [Math] "succ" $ inTags "mo" [] "&#x227B;"
  newCommand [Math] "preceq" $ inTags "mo" [] "&#x227C;"
  newCommand [Math] "succeq" $ inTags "mo" [] "&#x227D;"
  newCommand [Math] "subset" $ inTags "mo" [] "&#x2282;"
  newCommand [Math] "supset" $ inTags "mo" [] "&#x2283;"
  newCommand [Math] "subseteq" $ inTags "mo" [] "&#x2286;"
  newCommand [Math] "supseteq" $ inTags "mo" [] "&#x2287;"
  newCommand [Math] "sqsubset" $ inTags "mo" [] "&#x228F;"
  newCommand [Math] "sqsupset" $ inTags "mo" [] "&#x2290;"
  newCommand [Math] "sqsubseteq" $ inTags "mo" [] "&#x2291;"
  newCommand [Math] "sqsupseteq" $ inTags "mo" [] "&#x2292;"
  newCommand [Math] "sim" $ inTags "mo" [] "&#x223C;"
  newCommand [Math] "simeq" $ inTags "mo" [] "&#x2243;"
  newCommand [Math] "approx" $ inTags "mo" [] "&#x2248;"
  newCommand [Math] "cong" $ inTags "mo" [] "&#x2245;"
  newCommand [Math] "Join" $ inTags "mo" [] "&#x22C8;"
  newCommand [Math] "bowtie" $ inTags "mo" [] "&#x22C8;"
  newCommand [Math] "in" $ inTags "mo" [] "&#x2208;"
  newCommand [Math] "ni" $ inTags "mo" [] "&#x220B;"
  newCommand [Math] "owns" $ inTags "mo" [] "&#x220B;"
  newCommand [Math] "propto" $ inTags "mo" [] "&#x221D;"
  newCommand [Math] "vdash" $ inTags "mo" [] "&#x22A2;"
  newCommand [Math] "dashv" $ inTags "mo" [] "&#x22A3;"
  newCommand [Math] "models" $ inTags "mo" [] "&#x22A8;"
  newCommand [Math] "perp" $ inTags "mo" [] "&#x22A5;"
  newCommand [Math] "smile" $ inTags "mo" [] "&#x2323;"
  newCommand [Math] "frown" $ inTags "mo" [] "&#x2322;"
  newCommand [Math] "asymp" $ inTags "mo" [] "&#x224D;"
  newCommand [Math] "notin" $ inTags "mo" [] "&#x2209;"
  newCommand [Math] "gets" $ inTags "mo" [] "&#x2190;"
  newCommand [Math] "leftarrow" $ inTags "mo" [] "&#x2190;"
  newCommand [Math] "to" $ inTags "mo" [] "&#x2192;"
  newCommand [Math] "rightarrow" $ inTags "mo" [] "&#x2192;"
  newCommand [Math] "leftrightarrow" $ inTags "mo" [] "&#x2194;"
  newCommand [Math] "uparrow" $ inTags "mo" [] "&#x2191;"
  newCommand [Math] "downarrow" $ inTags "mo" [] "&#x2193;"
  newCommand [Math] "updownarrow" $ inTags "mo" [] "&#x2195;"
  newCommand [Math] "Leftarrow" $ inTags "mo" [] "&#x21D0;"
  newCommand [Math] "Rightarrow" $ inTags "mo" [] "&#x21D2;"
  newCommand [Math] "Leftrightarrow" $ inTags "mo" [] "&#x21D4;"
  newCommand [Math] "iff" $ inTags "mo" [] "&#x21D4;"
  newCommand [Math] "Uparrow" $ inTags "mo" [] "&#x21D1;"
  newCommand [Math] "Downarrow" $ inTags "mo" [] "&#x21D3;"
  newCommand [Math] "Updownarrow" $ inTags "mo" [] "&#x21D5;"
  newCommand [Math] "mapsto" $ inTags "mo" [] "&#x21A6;"
  newCommand [Math] "longleftarrow" $ inTags "mo" [] "&#x2190;"
  newCommand [Math] "longrightarrow" $ inTags "mo" [] "&#x2192;"
  newCommand [Math] "longleftrightarrow" $ inTags "mo" [] "&#x2194;"
  newCommand [Math] "Longleftarrow" $ inTags "mo" [] "&#x21D0;"
  newCommand [Math] "Longrightarrow" $ inTags "mo" [] "&#x21D2;"
  newCommand [Math] "Longleftrightarrow" $ inTags "mo" [] "&#x21D4;"
  newCommand [Math] "longmapsto" $ inTags "mo" [] "&#x21A6;"
  newCommand [Math] "sum" $ inTags "mo" [] "&#x2211;"
  newCommand [Math] "prod" $ inTags "mo" [] "&#x220F;"
  newCommand [Math] "bigcap" $ inTags "mo" [] "&#x22C2;"
  newCommand [Math] "bigcup" $ inTags "mo" [] "&#x22C3;"
  newCommand [Math] "bigwedge" $ inTags "mo" [] "&#x22C0;"
  newCommand [Math] "bigvee" $ inTags "mo" [] "&#x22C1;"
  newCommand [Math] "bigsqcap" $ inTags "mo" [] "&#x2A05;"
  newCommand [Math] "bigsqcup" $ inTags "mo" [] "&#x2A06;"
  newCommand [Math] "coprod" $ inTags "mo" [] "&#x2210;"
  newCommand [Math] "bigoplus" $ inTags "mo" [] "&#x2A01;"
  newCommand [Math] "bigotimes" $ inTags "mo" [] "&#x2A02;"
  newCommand [Math] "bigodot" $ inTags "mo" [] "&#x2A00;"
  newCommand [Math] "biguplus" $ inTags "mo" [] "&#x2A04;"
  newCommand [Math] "int" $ inTags "mo" [] "&#x222B;"
  newCommand [Math] "iint" $ inTags "mo" [] "&#x222C;"
  newCommand [Math] "iiint" $ inTags "mo" [] "&#x222D;"
  newCommand [Math] "oint" $ inTags "mo" [] "&#x222E;"
  newCommand [Math] "prime" $ inTags "mo" [] "&#x2032;"
  newCommand [Math] "dots" $ inTags "mo" [] "&#x2026;"
  newCommand [Math] "ldots" $ inTags "mo" [] "&#x2026;"
  newCommand [Math] "cdots" $ inTags "mo" [] "&#x22EF;"
  newCommand [Math] "vdots" $ inTags "mo" [] "&#x22EE;"
  newCommand [Math] "ddots" $ inTags "mo" [] "&#x22F1;"
  newCommand [Math] "forall" $ inTags "mo" [] "&#x2200;"
  newCommand [Math] "exists" $ inTags "mo" [] "&#x2203;"
  newCommand [Math] "Re" $ inTags "mo" [] "&#x211C;"
  newCommand [Math] "Im" $ inTags "mo" [] "&#x2111;"
  newCommand [Math] "aleph" $ inTags "mo" [] "&#x2135;"
  newCommand [Math] "hbar" $ inTags "mo" [] "&#x210F;"
  newCommand [Math] "ell" $ inTags "mo" [] "&#x2113;"
  newCommand [Math] "wp" $ inTags "mo" [] "&#x2118;"
  newCommand [Math] "emptyset" $ inTags "mo" [] "&#x2205;"
  newCommand [Math] "infty" $ inTags "mo" [] "&#x221E;"
  newCommand [Math] "partial" $ inTags "mo" [] "&#x2202;"
  newCommand [Math] "nabla" $ inTags "mo" [] "&#x2207;"
  newCommand [Math] "triangle" $ inTags "mo" [] "&#x25B3;"
  newCommand [Math] "therefore" $ inTags "mo" [] "&#x2234;"
  newCommand [Math] "angle" $ inTags "mo" [] "&#x2220;"
  newCommand [Math] "diamond" $ inTags "mo" [] "&#x22C4;"
  newCommand [Math] "Diamond" $ inTags "mo" [] "&#x25C7;"
  newCommand [Math] "lozenge" $ inTags "mo" [] "&#x25CA;"
  newCommand [Math] "neg" $ inTags "mo" [] "&#x00AC;"
  newCommand [Math] "lnot" $ inTags "mo" [] "&#x00AC;"
  newCommand [Math] "bot" $ inTags "mo" [] "&#x22A5;"
  newCommand [Math] "top" $ inTags "mo" [] "&#x22A4;"
  newCommand [Math] "square" $ inTags "mo" [] "&#x25AB;"
  newCommand [Math] "Box" $ inTags "mo" [] "&#x25A1;"
  newCommand [Math] "wr" $ inTags "mo" [] "&#x2240;"
  newCommand [Math] "arccos" $ inTags "mi" [] "arccos"
  newCommand [Math] "arcsin" $ inTags "mi" [] "arcsin"
  newCommand [Math] "arctan" $ inTags "mi" [] "arctan"
  newCommand [Math] "arg" $ inTags "mi" [] "arg"
  newCommand [Math] "cos" $ inTags "mi" [] "cos"
  newCommand [Math] "cosh" $ inTags "mi" [] "cosh"
  newCommand [Math] "cot" $ inTags "mi" [] "cot"
  newCommand [Math] "coth" $ inTags "mi" [] "coth"
  newCommand [Math] "csc" $ inTags "mi" [] "csc"
  newCommand [Math] "deg" $ inTags "mi" [] "deg"
  newCommand [Math] "det" $ inTags "mi" [] "det"
  newCommand [Math] "dim" $ inTags "mi" [] "dim"
  newCommand [Math] "exp" $ inTags "mi" [] "exp"
  newCommand [Math] "gcd" $ inTags "mi" [] "gcd"
  newCommand [Math] "hom" $ inTags "mi" [] "hom"
  newCommand [Math] "inf" $ inTags "mi" [] "inf"
  newCommand [Math] "ker" $ inTags "mi" [] "ker"
  newCommand [Math] "lg" $ inTags "mi" [] "lg"
  newCommand [Math] "lim" $ inTags "mi" [] "lim"
  newCommand [Math] "liminf" $ inTags "mi" [] "liminf"
  newCommand [Math] "limsup" $ inTags "mi" [] "limsup"
  newCommand [Math] "ln" $ inTags "mi" [] "ln"
  newCommand [Math] "log" $ inTags "mi" [] "log"
  newCommand [Math] "max" $ inTags "mi" [] "max"
  newCommand [Math] "min" $ inTags "mi" [] "min"
  newCommand [Math] "Pr" $ inTags "mi" [] "Pr"
  newCommand [Math] "sec" $ inTags "mi" [] "sec"
  newCommand [Math] "sin" $ inTags "mi" [] "sin"
  newCommand [Math] "sinh" $ inTags "mi" [] "sinh"
  newCommand [Math] "sup" $ inTags "mi" [] "sup"
  newCommand [Math] "tan" $ inTags "mi" [] "tan"
  newCommand [Math] "tanh" $ inTags "mi" [] "tanh"
  newCommand [Math] "setminus" $ inTags "mo" [] "\\"
  newCommand [Math] "!" $ inTags "mspace" [("width","-0.167em")] mempty
  newCommand [Math] "," $ inTags "mspace" [("width","0.167em")] mempty
  newCommand [Math] "," $ inTags "mspace" [("width","0.167em")] mempty
  newCommand [Math] ">" $ inTags "mspace" [("width","0.222em")] mempty
  newCommand [Math] ":" $ inTags "mspace" [("width","0.222em")] mempty
  newCommand [Math] ";" $ inTags "mspace" [("width","0.278em")] mempty
  newCommand [Math] "quad" $ inTags "mspace" [("width","1em")] mempty
  newCommand [Math] "qquad" $ inTags "mspace" [("width","2em")] mempty
  newCommand [Math] "frac" $ \(MathDoc x) (MathDoc y) ->
    inTags "mfrac" [] (x +++ y)
  newCommand [Math] "tfrac" $ \(MathDoc x) (MathDoc y) ->
    inTags "mstyle" [("displaystyle","false")] $ inTags "mfrac" [] (x +++ y)
  newCommand [Math] "dfrac" $ \(MathDoc x) (MathDoc y) ->
    inTags "mstyle" [("displaystyle","true")] $ inTags "mfrac" [] (x +++ y)
  newCommand [Math] "stackrel" $ \(MathDoc x) (MathDoc y) ->
    inTags "mover" [] (x +++ y)
  newCommand [Math] "overset" $ \(MathDoc x) (MathDoc y) ->
    inTags "mover" [] (x +++ y)
  newCommand [Math] "underset" $ \(MathDoc x) (MathDoc y) ->
    inTags "munder" [] (x +++ y)
  newCommand [Math] "binom" $ \(MathDoc x) (MathDoc y) ->
    inTags "mfenced" [] $ inTags "mfrac" [("linethickness","0")] (x +++ y)
  arrayEnv "bmatrix" $ matrix '[' ']'
  arrayEnv "pmatrix" $ matrix '(' ')'
  arrayEnv "Bmatrix" $ matrix '{' '}'
  arrayEnv "vmatrix" $ matrix '\x2223' '\x2223'
  arrayEnv "Vmatrix" $ matrix '\x2225' '\x2225'

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
  spaces
  modif <- try (string "\\left" >> return "left")
        <|> try (string "\\right" >> return "right")
        <|> scaler
        <|> return ""
  spaces
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
subsup :: HeX Doc -> HeX Doc
subsup parser = do
  res <- parser
  resStr <- case res of
                 Doc x -> return $ toString $ toLazyByteString x
                 Fut _ -> error "Unexpected Fut in math mode"
  limits <- Just <$> limitsIndicator <|> return Nothing
  spaces
  sub <- Just <$> subscript <|> return Nothing
  spaces
  sup <- Just <$> superscript <|> return Nothing
  spaces
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
subscript = char '_' >> math

superscript :: HeX Doc
superscript = char '^' >> math

data Alignment = AlignCenter | AlignLeft | AlignRight | AlignDefault

arrayEnv :: String -> ([Alignment] -> [[Doc]] -> Doc) -> HeX ()
arrayEnv s f =
   newEnvironment [Math] s $ \(mbopt :: Maybe String) -> do
     let aligns = alignsFromOpt mbopt
     lns <- arrayLines math
     return $ f aligns lns

alignsFromOpt :: Maybe String -> [Alignment]
alignsFromOpt (Just s) | all (`elem` "lrc") s = map go s
  where go c = case c of
                 'l' -> AlignLeft
                 'r' -> AlignRight
                 'c' -> AlignCenter
                 _   -> error $ "Unexpected align character " ++ ['`',c,'\'']
alignsFromOpt _ = repeat AlignDefault

matrix :: Char -> Char -> [Alignment] -> [[Doc]] -> Doc
matrix opendelim closedelim aligns rows =
  mrow $ stretchy opendelim +++ arrayRows aligns rows +++ stretchy closedelim

arrayRows :: [Alignment] -> [[Doc]] -> Doc
arrayRows aligns = mconcat . map (arrayRow aligns)

arrayRow :: [Alignment] -> [Doc] -> Doc
arrayRow aligns cells =
  inTags "mtr" [] $ mconcat $ zipWith arrayCell aligns cells

arrayCell :: Alignment -> Doc -> Doc
arrayCell align cell = inTags "mtd" align' cell
  where align' = case align of
                      AlignDefault -> []
                      AlignLeft    -> [("columnalign","left")]
                      AlignRight   -> [("columnalign","right")]
                      AlignCenter  -> [("columnalign","center")]

stretchy :: Char -> Doc
stretchy c = inTags "mo" [("stretchy","true")] $ rawc c



{-




endLine :: GenParser Char st Char
endLine = try $ do
  symbol "\\\\"
  optional inbrackets  -- can contain e.g. [1.0in] for a line height, not yet supported
  return '\n'

arrayLine :: GenParser Char st ArrayLine
arrayLine = notFollowedBy (try $ char '\\' >> symbol "end" >> return '\n') >>
  sepBy1 (many (notFollowedBy endLine >> expr)) (symbol "&")

array :: GenParser Char st Exp
array = stdarray <|> eqnarray <|> align <|> cases <|> matrix


stdarray :: GenParser Char st Exp
stdarray = inEnvironment "array" $ do
  aligns <- option [] arrayAlignments
  liftM (EArray aligns) $ sepEndBy1 arrayLine endLine

eqnarray :: GenParser Char st Exp
eqnarray = inEnvironment "eqnarray" $
  liftM (EArray [AlignRight, AlignCenter, AlignLeft]) $
    sepEndBy1 arrayLine endLine

align :: GenParser Char st Exp
align = inEnvironment "align" $
  liftM (EArray [AlignRight, AlignLeft]) $
    sepEndBy1 arrayLine endLine

cases :: GenParser Char st Exp
cases = inEnvironment "cases" $ do
  rs <- sepEndBy1 arrayLine endLine
  return $ EGrouped [EStretchy (ESymbol Open "{"), EArray [] rs]


inEnvironment :: String
              -> GenParser Char st Exp
              -> GenParser Char st Exp
inEnvironment envType p = do
  try $ do char '\\'
           symbol "begin"
           braces $ symbol envType >> optional (symbol "*")
  result <- p
  char '\\'
  symbol "end"
  braces $ symbol envType >> optional (symbol "*")
  return result 



-}

