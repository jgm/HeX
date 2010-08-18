{-# LANGUAGE OverloadedStrings #-}
import Language.Haskell.Interpreter
import System.Environment
import qualified Data.ByteString.Lazy.Char8 as L
import Data.ByteString.Lazy.UTF8 (toString)
import System.Exit
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO
import Control.Exception (finally)
import Control.Monad
import Data.List.Split
import Data.List (intercalate)

main :: IO ()
main = do
  args <- getArgs
  (file,fmt) <- case args of
                     [x,y]   -> return (x,y)
                     _       -> do hPutStrLn stderr $ "Usage:  hex FILE FORMAT"
                                   exitWith $ ExitFailure 1
  (code, txt) <- liftM splitSource $ L.readFile file
  withTempFile "hextemph.lhs" $ \fp h -> do
    L.hPut h code
    hClose h
    res <- runInterpreter $ do
      loadModules [fp]
      setTopLevelModules ["Main"]
      setImports ["Data.ByteString.Lazy.Internal"]
      interpret ("run parsers") (as :: String -> String -> IO L.ByteString)
    case res of
         Left (UnknownError s)    -> error s
         Left (WontCompile (e:_)) -> error $ replace fp file $ errMsg e
         Left (NotAllowed s)      -> error s
         Left (GhcException s)    -> error s
         Left err                 -> error $ show err
         Right f                  -> L.putStr =<< f fmt (toString txt)
    exitWith ExitSuccess

withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile patt f = do
  tempdir <- catch getTemporaryDirectory (\_ -> return ".")
  (tempfile, temph) <- openTempFile tempdir patt
  finally (f tempfile temph) (hClose temph >> removeFile tempfile)

splitSource :: L.ByteString -> (L.ByteString, L.ByteString)
splitSource src = (code, txt)
  where code = L.unlines $ map (zeroIf (not . isCodeLine)) srclines
        txt  = L.unlines $ map (zeroIf isCodeLine) srclines
        srclines = L.lines src
        isCodeLine = L.isPrefixOf "> "
        zeroIf test ln = if test ln then "" else ln

replace :: String -> String -> String -> String
replace target replacement = intercalate replacement . splitOn target
