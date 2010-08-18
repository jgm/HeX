import Language.Haskell.Interpreter
import System.Environment
import qualified Data.ByteString.Lazy as L
import System.Exit
import System.IO

main :: IO ()
main = do
  args <- getArgs
  (file,fmt) <- case args of
                     [x,y]   -> return (x,y)
                     _       -> do hPutStrLn stderr $ "Usage:  hex FILE FORMAT"
                                   exitWith $ ExitFailure 1
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
  exitWith ExitSuccess
