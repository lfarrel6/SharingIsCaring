{-# LANGUAGE RecordWildCards #-}

module DirectoryServer
    ( someFunc
    ) where

import FileServer
import File
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Concurrent.STM
import Network

someFunc :: IO ()
someFunc = do
  newDS <- newDirectory
  addFileServer newDS
  showDS newDS
  testServer newDS

type DirectoryServer = TVar (Map Int FileServer)

newDirectory :: IO DirectoryServer
newDirectory = newTVarIO Map.empty

showDS :: DirectoryServer -> IO ()
showDS ds = putStrLn $ ">Directory Server\n"

addFileServer :: DirectoryServer -> IO ()
addFileServer ds = do
  -- create and reply
  createFS >> putStrLn "New File Server Created" 
  where
   createFS = atomically $ do
    serverDir <- readTVar ds
    newServer <- newFileServer 0 []
    let newServerDir  = Map.insert 0 newServer serverDir
    writeTVar ds newServerDir

testServer :: DirectoryServer -> IO ()
testServer ds = do
  fs <- atomically $ readTVar ds
  case Map.lookup 0 fs of
   Nothing      -> putStrLn "weird, file server not found"
   Just fserver -> addFile fserver $ newFile "../data/test.txt"