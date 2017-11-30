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
    startServer ds (directoryPort cfg) (startPort cfg) (nServers cfg)
   Nothing  -> putStrLn "Bad Parameters."