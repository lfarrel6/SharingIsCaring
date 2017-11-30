module Main where

import DirectoryServer
import Config
import System.Environment

main :: IO ()
main = do
  received <- getConfig
  case received of
   Just cfg -> do
    --putStrLn "Successful invocation"
    ds <- newDirectory
    startServer ds (directoryPort cfg) 0 0
   Nothing  -> putStrLn "Bad Parameters."