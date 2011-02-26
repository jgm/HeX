{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Text.HeX.Math.MathML (defaults) where

import Text.HeX
import Text.HeX.Standard.Xml
import Control.Applicative ((<$>))
import Text.HeX.Math (defaultsFor, withText)
import qualified Data.Map as M

defaults :: HeX ()
defaults = do
  defaultsFor writer
  addParser [Math] enclosure
  register [Math] "textrm" $ asText "normal" <$> withText
  register [Math] "text" $ asText "normal" <$> withText
  register [Math] "mathrm" $ asText "normal" <$> getNext
  register [Math] "mbox" $ asText "normal" <$> getNext
  register [Math] "mathit" $ asText "italic" <$> getNext
  register [Math] "textit" $ asText "italic" <$> withText
  register [Math] "mathtt" $ asText "monospace" <$> getNext
  register [Math] "texttt" $ asText "monospace" <$> withText
  register [Math] "mathsf" $ asText "sans-serif" <$> getNext
  register [Math] "mathbb" $ asText "double-struck" <$> getNext
  register [Math] "mathcal" $ asText "script" <$> getNext
  register [Math] "mathfrak" $ asText "fraktur" <$> getNext
  register [Math] "sqrt" root
  register [Math] "surd" root
  register [Math] "acute" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#180;"
  register [Math] "grave" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "`"
  register [Math] "breve" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#728;"
  register [Math] "check" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#711;"
  register [Math] "dot" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "."
  register [Math] "ddot" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] ".."
  register [Math] "mathring" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#176;"
  register [Math] "vec" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#8407;"
  register [Math] "overrightarrow" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#8407;"
  register [Math] "overleftarrow" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#8406;"
  register [Math] "hat" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "^"
  register [Math] "widehat" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#770;"
  register [Math] "tilde" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "~"
  register [Math] "widetilde" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#732;"
  register [Math] "bar" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#8254;"
  register [Math] "overbrace" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#65079;"
  register [Math] "overbracket" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#9140;"
  register [Math] "overline" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#175;"
  register [Math] "underbrace" $ \d ->
    inTags "munder" [] $ d +++ inTags "mo" [("accent","true")] "&#65080;"
  register [Math] "underbracket" $ \d ->
    inTags "munder" [] $ d +++ inTags "mo" [("accent","true")] "&#9141;"
  register [Math] "underline" $ \d ->
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
  register [Math] "frac" $ \x y -> inTags "mfrac" [] (x +++ y)
  register [Math] "tfrac" $ \x y -> inTags "mstyle" [("displaystyle","false")]
                             $ inTags "mfrac" [] (x +++ y)
  register [Math] "dfrac" $ \x y -> inTags "mstyle" [("displaystyle","true")]
                             $ inTags "mfrac" [] (x +++ y)
  register [Math] "stackrel" $ \x y -> inTags "mover" [] (x +++ y)
  register [Math] "overset" $ \x y -> inTags "mover" [] (x +++ y)
  register [Math] "underset" $ \x y -> inTags "munder" [] (x +++ y)
  register [Math] "binom" $ \x y -> inTags "mfenced" []
                             $ inTags "mfrac" [("linethickness","0")] (x +++ y)

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

root :: Maybe Doc -> Doc -> Doc
root Nothing y  = inTags "msqrt" [] $ inTags "mn" [] y
root (Just x) y = inTags "mroot" [] $ inTags "mn" [] y +++ inTags "mn" [] x

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
enclosure = basicEnclosure -- TODO : <|> left <|> right <|> scaledEnclosure

basicEnclosure :: HeX Doc
basicEnclosure = inTags "mo" [] . rawc <$>
                   (oneOf "[]()" <|> (char '|' >> return '\x2223'))
              <|> try (do char '\\'
                          cmd <- many1 letter <|> count 1 anyChar
                          case M.lookup cmd enclosures of
                                Just x   -> return $ inTags "mo" [] $ rawc x
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

{-
basicEnclosure :: GenParser Char st Exp
basicEnclosure = choice $ map (\(s, v) -> try (symbol s) >> return v) enclosures

left :: GenParser Char st Exp
left = try $ do
  symbol "\\left"
  enc <- basicEnclosure <|> (try (symbol ".") >> return (ESymbol Open "\xFEFF"))
  case enc of
    (ESymbol Open _) -> tilRight enc <|> return (EStretchy enc)
    _ -> pzero

right :: GenParser Char st Exp
right = try $ do
  symbol "\\right"
  enc <- basicEnclosure <|> (try (symbol ".") >> return (ESymbol Close "\xFEFF"))
  case enc of
    (ESymbol Close x) -> return (EStretchy $ ESymbol Open x)
    _ -> pzero

-- We want stuff between \left( and \right) to be in an mrow,
-- so that the scaling is based just on this unit, and not the
-- whole containing formula.
tilRight :: Exp -> GenParser Char st Exp
tilRight start = try $ do
  contents <- manyTill expr
               (try $ symbol "\\right" >> lookAhead basicEnclosure)
  end <- basicEnclosure
  return $ EGrouped $ EStretchy start : (contents ++ [EStretchy end])

scaledEnclosure :: GenParser Char st Exp
scaledEnclosure = try $ do
  cmd <- command
  case M.lookup cmd scalers of
       Just  r -> liftM (EScaled r . EStretchy) basicEnclosure
       Nothing -> pzero 

scalers :: M.Map String String
scalers = M.fromList
          [ ("\\bigg", "2.2")
          , ("\\Bigg", "2.9")
          , ("\\big", "1.2")
          , ("\\Big", "1.6")
          , ("\\biggr", "2.2")
          , ("\\Biggr", "2.9")
          , ("\\bigr", "1.2")
          , ("\\Bigr", "1.6")
          , ("\\biggl", "2.2")
          , ("\\Biggl", "2.9")
          , ("\\bigl", "1.2")
          , ("\\Bigl", "1.6")
          ]




-}
