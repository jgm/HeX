{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Text.HeX.Math.MathML (defaults) where

import Text.HeX
import Text.HeX.Standard.Xml
import Control.Applicative ((<$>))
import Text.HeX.Math (defaultsFor, withText)

defaults :: HeX ()
defaults = do
  defaultsFor writer
  register "textrm" $ asText "normal" <$> withText
  register "text" $ asText "normal" <$> withText
  register "mathrm" $ asText "normal" <$> getNext
  register "mbox" $ asText "normal" <$> getNext
  register "mathit" $ asText "italic" <$> getNext
  register "textit" $ asText "italic" <$> withText
  register "mathtt" $ asText "monospace" <$> getNext
  register "texttt" $ asText "monospace" <$> withText
  register "mathsf" $ asText "sans-serif" <$> getNext
  register "mathbb" $ asText "double-struck" <$> getNext
  register "mathcal" $ asText "script" <$> getNext
  register "mathfrak" $ asText "fraktur" <$> getNext
  register "sqrt" root
  register "surd" root
  register "acute" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#180;"
  register "grave" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "`"
  register "breve" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#728;"
  register "check" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#711;"
  register "dot" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "."
  register "ddot" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] ".."
  register "mathring" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#176;"
  register "vec" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#8407;"
  register "overrightarrow" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#8407;"
  register "overleftarrow" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#8406;"
  register "hat" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "^"
  register "widehat" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#770;"
  register "tilde" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "~"
  register "widetilde" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#732;"
  register "bar" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#8254;"
  register "overbrace" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#65079;"
  register "overbracket" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#9140;"
  register "overline" $ \d ->
    mover $ d +++ inTags "mo" [("accent","true")] "&#175;"
  register "underbrace" $ \d ->
    inTags "munder" [] $ d +++ inTags "mo" [("accent","true")] "&#65080;"
  register "underbracket" $ \d ->
    inTags "munder" [] $ d +++ inTags "mo" [("accent","true")] "&#9141;"
  register "underline" $ \d ->
    inTags "munder" [] $ d +++ inTags "mo" [("accent","true")] "&#175;"
  register "mid" $ inTags "mo" [] "&#x2223;"
  register "parallel" $ inTags "mo" [] "&#x2225;"
  register "backslash" $ inTags "mo" [] "&#x2216;"
  register "times" $ inTags "mo" [] "&#x00D7;"
  register "alpha" $ inTags "mo" [] "&#x03B1;"
  register "beta" $ inTags "mo" [] "&#x03B2;"
  register "chi" $ inTags "mo" [] "&#x03C7;"
  register "delta" $ inTags "mo" [] "&#x03B4;"
  register "Delta" $ inTags "mo" [] "&#x0394;"
  register "epsilon" $ inTags "mo" [] "&#x03B5;"
  register "varepsilon" $ inTags "mo" [] "&#x025B;"
  register "eta" $ inTags "mo" [] "&#x03B7;"
  register "gamma" $ inTags "mo" [] "&#x03B3;"
  register "Gamma" $ inTags "mo" [] "&#x0393;" 
  register "iota" $ inTags "mo" [] "&#x03B9;"
  register "kappa" $ inTags "mo" [] "&#x03BA;"
  register "lambda" $ inTags "mo" [] "&#x03BB;"
  register "Lambda" $ inTags "mo" [] "&#x039B;" 
  register "mu" $ inTags "mo" [] "&#x03BC;"
  register "nu" $ inTags "mo" [] "&#x03BD;"
  register "omega" $ inTags "mo" [] "&#x03C9;"
  register "Omega" $ inTags "mo" [] "&#x03A9;"
  register "phi" $ inTags "mo" [] "&#x03C6;"
  register "varphi" $ inTags "mo" [] "&#x03D5;"
  register "Phi" $ inTags "mo" [] "&#x03A6;" 
  register "pi" $ inTags "mo" [] "&#x03C0;"
  register "Pi" $ inTags "mo" [] "&#x03A0;" 
  register "psi" $ inTags "mo" [] "&#x03C8;"
  register "Psi" $ inTags "mo" [] "&#x03A8;"
  register "rho" $ inTags "mo" [] "&#x03C1;"
  register "sigma" $ inTags "mo" [] "&#x03C3;"
  register "Sigma" $ inTags "mo" [] "&#x03A3;" 
  register "tau" $ inTags "mo" [] "&#x03C4;"
  register "theta" $ inTags "mo" [] "&#x03B8;"
  register "vartheta" $ inTags "mo" [] "&#x03D1;"
  register "Theta" $ inTags "mo" [] "&#x0398;" 
  register "upsilon" $ inTags "mo" [] "&#x03C5;"
  register "xi" $ inTags "mo" [] "&#x03BE;"
  register "Xi" $ inTags "mo" [] "&#x039E;" 
  register "zeta" $ inTags "mo" [] "&#x03B6;"
  register "frac12" $ inTags "mo" [] "&#x00BD;"
  register "frac14" $ inTags "mo" [] "&#x00BC;"
  register "frac34" $ inTags "mo" [] "&#x00BE;"
  register "frac13" $ inTags "mo" [] "&#x2153;"
  register "frac23" $ inTags "mo" [] "&#x2154;"
  register "frac15" $ inTags "mo" [] "&#x2155;"
  register "frac25" $ inTags "mo" [] "&#x2156;"
  register "frac35" $ inTags "mo" [] "&#x2157;"
  register "frac45" $ inTags "mo" [] "&#x2158;"
  register "frac16" $ inTags "mo" [] "&#x2159;"
  register "frac56" $ inTags "mo" [] "&#x215A;"
  register "frac18" $ inTags "mo" [] "&#x215B;"
  register "frac38" $ inTags "mo" [] "&#x215C;"
  register "frac58" $ inTags "mo" [] "&#x215D;"
  register "frac78" $ inTags "mo" [] "&#x215E;"
  register "pm" $ inTags "mo" [] "&#x00B1;"
  register "mp" $ inTags "mo" [] "&#x2213;"
  register "triangleleft" $ inTags "mo" [] "&#x22B2;"
  register "triangleright" $ inTags "mo" [] "&#x22B3;"
  register "cdot" $ inTags "mo" [] "&#x22C5;"
  register "star" $ inTags "mo" [] "&#x22C6;"
  register "ast" $ inTags "mo" [] "&#x002A;"
  register "times" $ inTags "mo" [] "&#x00D7;"
  register "div" $ inTags "mo" [] "&#x00F7;"
  register "circ" $ inTags "mo" [] "&#x2218;"
  register "bullet" $ inTags "mo" [] "&#x2022;"
  register "oplus" $ inTags "mo" [] "&#x2295;"
  register "ominus" $ inTags "mo" [] "&#x2296;"
  register "otimes" $ inTags "mo" [] "&#x2297;"
  register "bigcirc" $ inTags "mo" [] "&#x25CB;"
  register "oslash" $ inTags "mo" [] "&#x2298;"
  register "odot" $ inTags "mo" [] "&#x2299;"
  register "land" $ inTags "mo" [] "&#x2227;"
  register "wedge" $ inTags "mo" [] "&#x2227;"
  register "lor" $ inTags "mo" [] "&#x2228;"
  register "vee" $ inTags "mo" [] "&#x2228;"
  register "cap" $ inTags "mo" [] "&#x2229;"
  register "cup" $ inTags "mo" [] "&#x222A;"
  register "sqcap" $ inTags "mo" [] "&#x2293;"
  register "sqcup" $ inTags "mo" [] "&#x2294;"
  register "uplus" $ inTags "mo" [] "&#x228E;"
  register "amalg" $ inTags "mo" [] "&#x2210;"
  register "bigtriangleup" $ inTags "mo" [] "&#x25B3;"
  register "bigtriangledown" $ inTags "mo" [] "&#x25BD;"
  register "dag" $ inTags "mo" [] "&#x2020;"
  register "dagger" $ inTags "mo" [] "&#x2020;"
  register "ddag" $ inTags "mo" [] "&#x2021;"
  register "ddagger" $ inTags "mo" [] "&#x2021;"
  register "lhd" $ inTags "mo" [] "&#x22B2;"
  register "rhd" $ inTags "mo" [] "&#x22B3;"
  register "unlhd" $ inTags "mo" [] "&#x22B4;"
  register "unrhd" $ inTags "mo" [] "&#x22B5;"
  register "lt" $ inTags "mo" [] "<"
  register "gt" $ inTags "mo" [] ">"
  register "ne" $ inTags "mo" [] "&#x2260;"
  register "neq" $ inTags "mo" [] "&#x2260;"
  register "le" $ inTags "mo" [] "&#x2264;"
  register "leq" $ inTags "mo" [] "&#x2264;"
  register "leqslant" $ inTags "mo" [] "&#x2264;"
  register "ge" $ inTags "mo" [] "&#x2265;"
  register "geq" $ inTags "mo" [] "&#x2265;"
  register "geqslant" $ inTags "mo" [] "&#x2265;"
  register "equiv" $ inTags "mo" [] "&#x2261;"
  register "ll" $ inTags "mo" [] "&#x226A;"
  register "gg" $ inTags "mo" [] "&#x226B;"
  register "doteq" $ inTags "mo" [] "&#x2250;"
  register "prec" $ inTags "mo" [] "&#x227A;"
  register "succ" $ inTags "mo" [] "&#x227B;"
  register "preceq" $ inTags "mo" [] "&#x227C;"
  register "succeq" $ inTags "mo" [] "&#x227D;"
  register "subset" $ inTags "mo" [] "&#x2282;"
  register "supset" $ inTags "mo" [] "&#x2283;"
  register "subseteq" $ inTags "mo" [] "&#x2286;"
  register "supseteq" $ inTags "mo" [] "&#x2287;"
  register "sqsubset" $ inTags "mo" [] "&#x228F;"
  register "sqsupset" $ inTags "mo" [] "&#x2290;"
  register "sqsubseteq" $ inTags "mo" [] "&#x2291;"
  register "sqsupseteq" $ inTags "mo" [] "&#x2292;"
  register "sim" $ inTags "mo" [] "&#x223C;"
  register "simeq" $ inTags "mo" [] "&#x2243;"
  register "approx" $ inTags "mo" [] "&#x2248;"
  register "cong" $ inTags "mo" [] "&#x2245;"
  register "Join" $ inTags "mo" [] "&#x22C8;"
  register "bowtie" $ inTags "mo" [] "&#x22C8;"
  register "in" $ inTags "mo" [] "&#x2208;"
  register "ni" $ inTags "mo" [] "&#x220B;"
  register "owns" $ inTags "mo" [] "&#x220B;"
  register "propto" $ inTags "mo" [] "&#x221D;"
  register "vdash" $ inTags "mo" [] "&#x22A2;"
  register "dashv" $ inTags "mo" [] "&#x22A3;"
  register "models" $ inTags "mo" [] "&#x22A8;"
  register "perp" $ inTags "mo" [] "&#x22A5;"
  register "smile" $ inTags "mo" [] "&#x2323;"
  register "frown" $ inTags "mo" [] "&#x2322;"
  register "asymp" $ inTags "mo" [] "&#x224D;"
  register "notin" $ inTags "mo" [] "&#x2209;"
  register "gets" $ inTags "mo" [] "&#x2190;"
  register "leftarrow" $ inTags "mo" [] "&#x2190;"
  register "to" $ inTags "mo" [] "&#x2192;"
  register "rightarrow" $ inTags "mo" [] "&#x2192;"
  register "leftrightarrow" $ inTags "mo" [] "&#x2194;"
  register "uparrow" $ inTags "mo" [] "&#x2191;"
  register "downarrow" $ inTags "mo" [] "&#x2193;"
  register "updownarrow" $ inTags "mo" [] "&#x2195;"
  register "Leftarrow" $ inTags "mo" [] "&#x21D0;"
  register "Rightarrow" $ inTags "mo" [] "&#x21D2;"
  register "Leftrightarrow" $ inTags "mo" [] "&#x21D4;"
  register "iff" $ inTags "mo" [] "&#x21D4;"
  register "Uparrow" $ inTags "mo" [] "&#x21D1;"
  register "Downarrow" $ inTags "mo" [] "&#x21D3;"
  register "Updownarrow" $ inTags "mo" [] "&#x21D5;"
  register "mapsto" $ inTags "mo" [] "&#x21A6;"
  register "longleftarrow" $ inTags "mo" [] "&#x2190;"
  register "longrightarrow" $ inTags "mo" [] "&#x2192;"
  register "longleftrightarrow" $ inTags "mo" [] "&#x2194;"
  register "Longleftarrow" $ inTags "mo" [] "&#x21D0;"
  register "Longrightarrow" $ inTags "mo" [] "&#x21D2;"
  register "Longleftrightarrow" $ inTags "mo" [] "&#x21D4;"
  register "longmapsto" $ inTags "mo" [] "&#x21A6;"
  register "sum" $ inTags "mo" [] "&#x2211;"
  register "prod" $ inTags "mo" [] "&#x220F;"
  register "bigcap" $ inTags "mo" [] "&#x22C2;"
  register "bigcup" $ inTags "mo" [] "&#x22C3;"
  register "bigwedge" $ inTags "mo" [] "&#x22C0;"
  register "bigvee" $ inTags "mo" [] "&#x22C1;"
  register "bigsqcap" $ inTags "mo" [] "&#x2A05;"
  register "bigsqcup" $ inTags "mo" [] "&#x2A06;"
  register "coprod" $ inTags "mo" [] "&#x2210;"
  register "bigoplus" $ inTags "mo" [] "&#x2A01;"
  register "bigotimes" $ inTags "mo" [] "&#x2A02;"
  register "bigodot" $ inTags "mo" [] "&#x2A00;"
  register "biguplus" $ inTags "mo" [] "&#x2A04;"
  register "int" $ inTags "mo" [] "&#x222B;"
  register "iint" $ inTags "mo" [] "&#x222C;"
  register "iiint" $ inTags "mo" [] "&#x222D;"
  register "oint" $ inTags "mo" [] "&#x222E;"
  register "prime" $ inTags "mo" [] "&#x2032;"
  register "dots" $ inTags "mo" [] "&#x2026;"
  register "ldots" $ inTags "mo" [] "&#x2026;"
  register "cdots" $ inTags "mo" [] "&#x22EF;"
  register "vdots" $ inTags "mo" [] "&#x22EE;"
  register "ddots" $ inTags "mo" [] "&#x22F1;"
  register "forall" $ inTags "mo" [] "&#x2200;"
  register "exists" $ inTags "mo" [] "&#x2203;"
  register "Re" $ inTags "mo" [] "&#x211C;"
  register "Im" $ inTags "mo" [] "&#x2111;"
  register "aleph" $ inTags "mo" [] "&#x2135;"
  register "hbar" $ inTags "mo" [] "&#x210F;"
  register "ell" $ inTags "mo" [] "&#x2113;"
  register "wp" $ inTags "mo" [] "&#x2118;"
  register "emptyset" $ inTags "mo" [] "&#x2205;"
  register "infty" $ inTags "mo" [] "&#x221E;"
  register "partial" $ inTags "mo" [] "&#x2202;"
  register "nabla" $ inTags "mo" [] "&#x2207;"
  register "triangle" $ inTags "mo" [] "&#x25B3;"
  register "therefore" $ inTags "mo" [] "&#x2234;"
  register "angle" $ inTags "mo" [] "&#x2220;"
  register "diamond" $ inTags "mo" [] "&#x22C4;"
  register "Diamond" $ inTags "mo" [] "&#x25C7;"
  register "lozenge" $ inTags "mo" [] "&#x25CA;"
  register "neg" $ inTags "mo" [] "&#x00AC;"
  register "lnot" $ inTags "mo" [] "&#x00AC;"
  register "bot" $ inTags "mo" [] "&#x22A5;"
  register "top" $ inTags "mo" [] "&#x22A4;"
  register "square" $ inTags "mo" [] "&#x25AB;"
  register "Box" $ inTags "mo" [] "&#x25A1;"
  register "wr" $ inTags "mo" [] "&#x2240;"
  register "arccos" $ inTags "mi" [] "arccos"
  register "arcsin" $ inTags "mi" [] "arcsin"
  register "arctan" $ inTags "mi" [] "arctan"
  register "arg" $ inTags "mi" [] "arg"
  register "cos" $ inTags "mi" [] "cos"
  register "cosh" $ inTags "mi" [] "cosh"
  register "cot" $ inTags "mi" [] "cot"
  register "coth" $ inTags "mi" [] "coth"
  register "csc" $ inTags "mi" [] "csc"
  register "deg" $ inTags "mi" [] "deg"
  register "det" $ inTags "mi" [] "det"
  register "dim" $ inTags "mi" [] "dim"
  register "exp" $ inTags "mi" [] "exp"
  register "gcd" $ inTags "mi" [] "gcd"
  register "hom" $ inTags "mi" [] "hom"
  register "inf" $ inTags "mi" [] "inf"
  register "ker" $ inTags "mi" [] "ker"
  register "lg" $ inTags "mi" [] "lg"
  register "lim" $ inTags "mi" [] "lim"
  register "liminf" $ inTags "mi" [] "liminf"
  register "limsup" $ inTags "mi" [] "limsup"
  register "ln" $ inTags "mi" [] "ln"
  register "log" $ inTags "mi" [] "log"
  register "max" $ inTags "mi" [] "max"
  register "min" $ inTags "mi" [] "min"
  register "Pr" $ inTags "mi" [] "Pr"
  register "sec" $ inTags "mi" [] "sec"
  register "sin" $ inTags "mi" [] "sin"
  register "sinh" $ inTags "mi" [] "sinh"
  register "sup" $ inTags "mi" [] "sup"
  register "tan" $ inTags "mi" [] "tan"
  register "tanh" $ inTags "mi" [] "tanh"
  register "setminus" $ inTags "mo" [] "\\"
  register "!" $ inTags "mspace" [("width","-0.167em")] mempty
  register "," $ inTags "mspace" [("width","0.167em")] mempty
  register "," $ inTags "mspace" [("width","0.167em")] mempty
  register ">" $ inTags "mspace" [("width","0.222em")] mempty
  register ":" $ inTags "mspace" [("width","0.222em")] mempty
  register ";" $ inTags "mspace" [("width","0.278em")] mempty
  register "quad" $ inTags "mspace" [("width","1em")] mempty
  register "qquad" $ inTags "mspace" [("width","2em")] mempty
  register "frac" $ \x y -> inTags "mfrac" [] (x +++ y)
  register "tfrac" $ \x y -> inTags "mstyle" [("displaystyle","false")]
                             $ inTags "mfrac" [] (x +++ y)
  register "dfrac" $ \x y -> inTags "mstyle" [("displaystyle","true")]
                             $ inTags "mfrac" [] (x +++ y)
  register "stackrel" $ \x y -> inTags "mover" [] (x +++ y)
  register "overset" $ \x y -> inTags "mover" [] (x +++ y)
  register "underset" $ \x y -> inTags "munder" [] (x +++ y)
  register "binom" $ \x y -> inTags "mfenced" []
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

{-
enclosure :: GenParser Char st Exp
enclosure = basicEnclosure <|> left <|> right <|> scaledEnclosure

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

enclosures :: [(String, Exp)]
enclosures = [ ("(", ESymbol Open "(")
             , (")", ESymbol Close ")")
             , ("[", ESymbol Open "[")
             , ("]", ESymbol Close "]")
             , ("\\{", ESymbol Open "{")
             , ("\\}", ESymbol Close "}")
             , ("\\lbrack", ESymbol Open "[")
             , ("\\lbrace", ESymbol Open "{")
             , ("\\rbrack", ESymbol Close "]")
             , ("\\rbrace", ESymbol Close "}")
             , ("\\llbracket", ESymbol Open "\x27E6")
             , ("\\rrbracket", ESymbol Close "\x27E7")
             , ("\\langle", ESymbol Open "\x27E8")
             , ("\\rangle", ESymbol Close "\x27E9")
             , ("\\lfloor", ESymbol Open "\x230A")
             , ("\\rfloor", ESymbol Close "\x230B")
             , ("\\lceil", ESymbol Open "\x2308")
             , ("\\rceil", ESymbol Close "\x2309")
             , ("|", ESymbol Open "\x2223")
             , ("|", ESymbol Close "\x2223")
             , ("\\|", ESymbol Open "\x2225")
             , ("\\|", ESymbol Close "\x2225")
             , ("\\lvert", ESymbol Open "\x7C")
             , ("\\rvert", ESymbol Close "\x7C")
             , ("\\vert", ESymbol Close "\x7C")
             , ("\\lVert", ESymbol Open "\x2225")
             , ("\\rVert", ESymbol Close "\x2225")
             , ("\\Vert", ESymbol Close "\x2016")
             , ("\\ulcorner", ESymbol Open "\x231C")
             , ("\\urcorner", ESymbol Close "\x231D")
             ]


-}
