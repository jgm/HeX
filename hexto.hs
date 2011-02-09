{-# LANGUAGE OverloadedStrings #-}
import System.Environment
import qualified Data.ByteString.Lazy.Char8 as L
import Data.ByteString.Lazy.UTF8 (toString)
import System.Exit
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO
import Control.Exception (finally)
import Control.Monad
import Data.List (intercalate)

main :: IO ()
main = do
  args <- getArgs
  print args
