import Language.Haskell.Interpreter
import System.Environment
import qualified Data.ByteString.Lazy as L

main :: IO ()
main = do
  (file:fmt:_) <- getArgs
  res <- runInterpreter $ do
    loadModules [file]
    setTopLevelModules ["Main"]
    setImports ["Data.ByteString.Lazy.Internal"]
    interpret ("use parsers") (as :: FilePath -> String -> IO L.ByteString)
  case res of
        Left (UnknownError s)    -> error s
        Left (WontCompile (e:_)) -> error $ errMsg e
        Left (NotAllowed s)      -> error s
        Left (GhcException s)    -> error s
        Left err                 -> error $ show err
        Right f                  -> L.putStr =<< f file fmt
