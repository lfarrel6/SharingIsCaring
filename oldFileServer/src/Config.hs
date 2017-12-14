module Config (Config(..) , getConfig) where

import System.Environment

data Config = Config
  { directoryPort :: Int
  , startPort     :: Int
  , nServers      :: Int
  }

getConfig :: IO (Maybe Config)
getConfig = do
  args <- getArgs
  case args of
   [dp, sp, ns] -> do
     putStrLn (confirmationString dp sp ns)
     return $ Just Config { directoryPort = read dp :: Int
                          , startPort     = read sp :: Int
                          , nServers      = read ns :: Int
                          }
   [dp, sp]     -> do
     putStrLn (confirmationString dp sp ns)
     return $ Just Config { directoryPort = read dp :: Int
                          , startPort     = read sp :: Int
                          , nServers      = read ns :: Int
                          }
     where
      ns = "1"
   _            -> return Nothing
  where
      confirmationString dp sp ns = "Initial Params\n\t>Directory Server @ " ++ dp ++ "\n\t>File Servers @ " ++ sp ++ "+\n\t>" ++ ns ++ " Server(s)"