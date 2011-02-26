{-# LANGUAGE PackageImports, OverloadedStrings #-}
import Text.HeX
import Text.HeX.Standard as Standard
import Docbook as Docbook
import Control.Monad (guard, liftM)
import qualified Data.Map as M
import Text.HeX.Standard.Xml (inTags)
import Text.HeX.Standard.TeX (ctl,grp)

main = defaultMain $ do
  Standard.defaults
  forFormat "docbook" Docbook.defaults
  register [Block,Inline] "silly" silly
  register [Inline] "lettrine" lettrine
  addParser [Math, Inline] unknown
  -- FOR DEBUGGING
  forFormat "html" $ addParser [Math] unknownChar
  parseDoc

silly :: OptionList -> Doc
silly (OptionList opts) =
  raws $ show opts

lettrine :: Format -> Doc -> Doc -> Doc
lettrine "html" x y = inTags "span" [("class","lettrine")] x +++ y
lettrine "latex" x y = ctl "lettrine" +++ grp [x] +++ grp [y]

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

-- CSS:
-- body { line-height: 1.3; }
-- .lettrine {font-size:3em; float: left; line-height: 1; margin-right: 0.1em;}

