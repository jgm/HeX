> import Prelude hiding (repeat, pi)
> import Text.HeX.Default
> import Text.HeX.TeX as TeX
> import Text.HeX.Html as Html

> emph :: Doc -> Format -> HeX Doc
> emph arg "html" = return $ inTags "em" [] arg
> emph arg "tex"  = return $ ctl "emph" +++ grp [arg]

> pi :: HeX Doc
> pi = ensureMath $ return "\\pi"

> repeat :: Maybe Int -> Doc -> Doc
> repeat (Just n) = cat . replicate n

> parsers = [ command "repeat" repeat
>           , command "pi" pi
>           , command "em" emph
>           , base ]

Here's the text & that text and some \em{emphasized text}.
And some math: $e=mc^2$. And some display math: $$e=mc^2$$.

\pi\ and $y = \pi$.

\repeat[3]{hi there! }

