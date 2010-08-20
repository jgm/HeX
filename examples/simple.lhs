> import Text.HeX.Default
> import Text.HeX.TeX as TeX
> import Text.HeX.Html as Html

> emph = command "emph" $ do
>   arg <- getNext
>   "html"  ==> inTags "em" [] arg &
>    "tex"  ==> ctl "emph" +++ grp [arg]

> parsers = [group, math, emph, oneChar]

Here's the text & that text and some \emph{emphasized text}.
And some math: $e=mc^2$. And some display math: $$e=mc^2$$.
