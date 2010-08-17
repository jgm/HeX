#!/usr/bin/env runghc

> import Text.HeX

> oneChar = do
>   c <- anyChar
>   "html"  ==> escapeHtmlChar c &
>    "tex"  ==> escapeTeXChar c

> escapeHtmlChar '&' = fromString "&amp;"
> escapeHtmlChar '<' = fromString "&lt;"
> escapeHtmlChar '>' = fromString "&gt;"
> escapeHtmlChar c   = fromChar c

> escapeTeXChar c | c `elem` "$#&%"     = fromString ['\\',c]
> escapeTeXChar c | c `elem` "&~\\{}_^" = fromString $ "{\\char`\\" ++ [c] ++ "}"
> escapeTeXChar c                       = fromChar c

> main = use [oneChar]

Here's the text & that text.

