{-# LANGUAGE PackageImports, OverloadedStrings #-}
import Text.HeX
import Text.HeX.Standard as Standard
import Docbook as Docbook
import Control.Monad (guard, liftM)
import Control.Applicative ((<$>))
import qualified Data.Map as M
import Text.HeX.Standard.Xml (inTags)
import Text.HeX.Standard.TeX (ctl,grp)
import Control.Monad.Trans (liftIO)

main = defaultMain $ do
  Standard.defaults
  forFormat "docbook" Docbook.defaults
  newCommand [Block,Inline] "silly*" silly
  newCommand [Inline] "lettrine" lettrine
  newEnvironment [Block] "crazy" crazy
  -- addParser [Math, Inline] unknown
  -- FOR DEBUGGING
  -- forFormat "html" $ addParser [Math] unknownChar
  format <- getFormat
  addHeader format <$> parseDoc

addHeader :: Format -> Doc -> Doc
addHeader "html" d =
  "<!DOCTYPE html>" +++
  inTags "html" []
    ((inTags "head" [] $
      inTags "meta" [("charset","utf-8")] mempty +++
      inTags "title" [] mempty) +++
     (inTags "body" [] d))
addHeader "latex" d =
  "\\documentclass{article}\n\\begin{document}\n" +++ d +++ "\n\\end{document}"

silly :: Maybe OptionList -> Doc
silly (Just (OptionList opts)) =
  raws $ show opts
silly Nothing = mempty

lettrine :: Format -> InlineDoc -> InlineDoc -> Doc
lettrine "html" (InlineDoc x) (InlineDoc y) =
  inTags "span" [("class","lettrine")] x +++ y
lettrine "latex" (InlineDoc x) (InlineDoc y) =
  ctl "lettrine" +++ grp [x] +++ grp [y]

crazy :: Maybe String -> HeX Doc
crazy Nothing = mconcat <$> many block
crazy (Just opt) = mconcat <$> many ((raws opt +++) <$> block)


{-
unknown :: HeX Doc
unknown = try $ do
  char '\\'
  cmd <- many1 letter <|> count 1 anyChar
  commands <- liftM hexCommands getState
  mode <- liftM hexMode getState
  case M.lookup (mode, cmd) commands of
       Nothing -> do
          case M.lookup (Block, cmd) commands of
               Just _ -> fail "block command"
               Nothing -> do
                 f <- getFormat
                 return $ case f of
                          "html" -> inTags "span" [("style","color:red")] (raws cmd)
                          _      -> "[" +++ raws cmd +++ "]"
       Just _  -> fail "known command"

unknownChar :: HeX Doc
unknownChar = do
  c <- oneOf "()[]|&"
  return $ inTags "span" [("style","color:red")] $ raws ['[',c,']']
-}
-- CSS:
-- body { line-height: 1.3; }
-- .lettrine {font-size:3em; float: left; line-height: 1; margin-right: 0.1em;}

