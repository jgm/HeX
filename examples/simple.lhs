> import Text.HeX.Default
> import Text.HeX.TeX as TeX
> import Text.HeX.Html as Html

> emph :: Format -> Doc -> HeX Doc
> emph "html" arg = return $ inTags "em" [] arg
> emph "tex"  arg = return $ ctl "emph" +++ grp [arg]

> funny :: Format -> HeX Doc
> funny _ = return "\\pi"

> two :: Format -> Doc -> Doc -> HeX Doc
> two _ x1 x2 = return $ x1 +++ ".." +++ x2

> parsers = [ group
>           , command "two" $ withArg $ withArg $ two
>           , command "funny" $ ensureMath funny
>           , math
>           , command "em" $ withArg emph
>           , oneChar]

Here's the text & that text and some \em{emphasized text}.
And some math: $e=mc^2$. And some display math: $$e=mc^2$$.

\funny and $y = \funny$.

\two{one}{two}

