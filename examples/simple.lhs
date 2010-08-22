> import Text.HeX.Default
> import Text.HeX.TeX as TeX
> import Text.HeX.Html as Html

> emph arg =
>   "html"  ==> inTags "em" [] arg &
>    "tex"  ==> ctl "emph" +++ grp [arg]

> funny =
>   "html"  ==> "\\pi"
>    & "tex" ==> "\\pi"

> -- Wouldn't be even better if it were:
> -- funny "html" = "\\pi"
> -- funny "tex"  = "\\pi"
> -- or funny _ = "\\pi"

> parsers = [ group
>           , command "funny" $ ensureMath funny
>           , math
>           , command "em" $ withArg emph
>           , oneChar]

Here's the text & that text and some \em{emphasized text}.
And some math: $e=mc^2$. And some display math: $$e=mc^2$$.

\funny and $y = \funny$.

