{-# LANGUAGE RecordWildCards #-}

module DirectoryServer
    ( someFunc , startServer , newDirectory ) where

import qualified FileServer as FS
import File
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import Network
import Network.Socket (close)
import System.IO
import System.Directory
import GHC.Conc

someFunc :: IO ()
someFunc = do
  newDS <- newDirectory
  createFileServer newDS 0 0
  showDS newDS

type DirectoryServer = TVar (Map Int FS.FileServer)

newDirectory :: IO DirectoryServer
newDirectory = newTVarIO Map.empty

showDS :: DirectoryServer -> IO ()
showDS ds = putStrLn ">Directory Server\n"

createFileServer :: DirectoryServer -> Int -> Int -> IO ()
createFileServer ds id portNum =
  -- create and reply
  createFS >> putStrLn "New File Server Created"
  where
   createFS = do
    serverDir <- atomically $ readTVar ds
    newServer <- FS.buildFileServer id portNum
    FS.startServer newServer
    let newServerDir  = Map.insert id newServer serverDir
    atomically $ writeTVar ds newServerDir

startServer :: DirectoryServer -> Int -> Int -> Int -> IO ()
startServer ds dsPort fsPort nFS = withSocketsDo $ do
  createFileServer ds 1 fsPort
  sock <- listenOn $ portNum dsPort
  putStrLn $ "\t>Directory Server starting on " ++ show dsPort
  listen sock
  where
   portNum n = PortNumber $ fromIntegral n
   listen s  = do
    (handle, host, clientPort) <- accept s
    hPutStrLn handle $ "\t>connected to ds @ " ++ show dsPort
    forkFinally (runServer ds handle) (\_ -> putStrLn "user disconnecting")
    listen s

runServer :: DirectoryServer -> Handle -> IO ()
runServer ds hdl = do
  hSetNewlineMode hdl universalNewlineMode
  hSetBuffering   hdl NoBuffering
  interact
  where
   interact = do
    request <- hGetLine hdl
    case words request of

     ["GET_FILE:", filename] -> do
       putStrLn $ "\t>" ++ filename ++ " requested"
       hPutStrLn hdl "here u go u filthy animal"
       interact

     ["CREATE_FILE:", newfilename] -> do
      putStrLn $ "\t>" ++ newfilename ++ " to be created"
      hPutStrLn hdl "wow demanding arent u"
      interact

     ["UPDATE_FILE:", filename] -> do
      putStrLn $ "\t>" ++ filename ++ " to be updated"
      hPutStrLn hdl "i like it how it is"
      interact

     ["QUIT"] -> putStrLn "lol weak ass kid" >> hPutStrLn hdl "lol bye bish"

     _ -> putStrLn "wildcard" >> interact