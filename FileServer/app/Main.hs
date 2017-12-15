module Main where

import FileServer (buildFileServer)
import Config (parseConfig)
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  cfg <- parseConfig args
  case cfg of
   Just config -> buildFileServer config
   Nothing -> putStrLn "bad input"