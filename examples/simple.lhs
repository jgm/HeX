> import Text.HeX
> import Text.HeX.TeX as TeX
> import Text.HeX.Html as Html

> oneChar = do
>   c <- anyChar
>   "html"  ==> Html.ch c & "tex" ==> TeX.ch c

> emph = command "emph" $ do
>   arg <- getNext
>   "html"  ==> inTags "em" [] arg &
>    "tex"  ==> ctl "emph" +++ grp [arg]

> group = do
>   char '{'
>   res <- manyTill getNext (char '}')
>   return $ mconcat res

> parsers = [group, emph, oneChar]

Here's the text & that text and some \emph{emphasized text}.

