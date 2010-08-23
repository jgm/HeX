> import Text.HeX.Default
> import Text.HeX.TeX as TeX
> import Text.HeX.Html as Html

> emph :: Doc -> Format -> HeX Doc
> emph arg "html" = return $ inTags "em" [] arg
> emph arg "tex"  = return $ ctl "emph" +++ grp [arg]

> funny :: Format -> HeX Doc
> funny _ = ensureMath $ return "\\pi"

> two :: Doc -> Doc -> Format -> HeX Doc
> two x1 x2 _ = return $ x1 +++ ".." +++ x2

> parsers = [ group
>           , command "two" two
>           , command "funny" funny
>           , math
>           , command "em" emph
>           , oneChar]

Here's the text & that text and some \em{emphasized text}.
And some math: $e=mc^2$. And some display math: $$e=mc^2$$.

\funny and $y = \funny$.

\two{one}{two}

