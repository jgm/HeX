> import Text.HeX
> import Text.HeX.TeX as TeX
> import Text.HeX.Html as Html

> oneChar = do
>   c <- anyChar
>   "html"  ==> Html.ch c & "tex" ==> TeX.ch c

> parsers = [oneChar]

Here's the text & that text.

