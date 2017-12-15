module Main where

import DirectoryServer (startApp)

import System.Environment

main :: IO ()
main = do
  input <- getArgs
  case input of
   [port] -> startApp (read port :: Int)
   _      -> putStrLn "bad input"
