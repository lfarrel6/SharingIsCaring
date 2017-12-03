{-# LANGUAGE RecordWildCards #-}

module DirectoryServer
    ( DirectoryServer , startServer , newDirectory , getAllFiles ) where

import qualified FileServer as FS
import File
import Message

import qualified Data.Map as Map
import Data.Map (Map)
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Network
import Network.Socket (close)
import System.IO
import System.Directory
import GHC.Conc
import Servant
import Data.Hashable

someFunc :: IO ()
someFunc = do
  newDS <- newDirectory
  showDS newDS

data DirectoryServer = DirectoryServer
  { fileToLocations :: TVar (Map Int [FS.FileServer]) --Hash of filenames to servers (first server in list has primary copy)
  , allServers      :: TVar (Map Int FS.FileServer)   --Server IDs to Servers
  , commsChan       :: TChan Message                  --TChan for receiving messages from file servers
  , nServers        :: TVar Int                       --Number of live File Servers
  , nextID          :: TVar Int                       --Next server ID
  }

newDirectory :: IO DirectoryServer
newDirectory = do
  ftl   <- atomically $ newTVar Map.empty
  as    <- atomically $ newTVar Map.empty
  comms <- newTChanIO
  ns    <- atomically $ newTVar 0
  return DirectoryServer { fileToLocations = ftl
                         , allServers      = as
                         , commsChan       = comms
                         , nServers        = ns
                         , nextID          = ns
                         }

showDS :: DirectoryServer -> IO ()
showDS ds = putStrLn ">Directory Server\n"

createFileServer :: DirectoryServer -> Int -> IO ()
createFileServer ds@DirectoryServer{..} portNum =
  -- create and reply
  createFS >> putStrLn "New File Server Created"
  where
   createFS = do
    serverDir   <- atomically $ readTVar allServers
    serverCount <- atomically $ readTVar nServers
    serverID    <- atomically $ readTVar nextID
    newServer   <- FS.buildFileServer serverID portNum
    FS.startServer newServer
    let newServerDir  = Map.insert serverID newServer serverDir
    atomically $ writeTVar allServers newServerDir
    atomically $ writeTVar nServers   (serverCount+1)
    atomically $ writeTVar nextID     (serverID+1)

startServer :: DirectoryServer -> Int -> Int -> Int -> IO ()
startServer ds dsPort fsPort nFS = withSocketsDo $ do
  initFileServers ds fsPort nFS
  sock <- listenOn $ portNum dsPort
  putStrLn $ "\t>Directory Server starting on " ++ show dsPort
  --listen sock
  where
   portNum n = PortNumber $ fromIntegral n
   {-listen s  = do
    (handle, host, clientPort) <- accept s
    hPutStrLn handle $ "\t>connected to ds @ " ++ show dsPort
    forkFinally (runServer ds handle) (\_ -> putStrLn "user disconnecting")
    listen s-}

initFileServers :: DirectoryServer -> Int -> Int -> IO ()
initFileServers _ _ 0          = putStrLn "File Servers created"
initFileServers ds startPort n = createFileServer ds startPort >> initFileServers ds (startPort+1) (n-1)

getAllFiles :: DirectoryServer -> Handler [File]
getAllFiles ds@DirectoryServer{..} = do
  servers <- liftIO $ atomically $ readTVar allServers
  let serverList = Map.elems servers
  head (map FS.getFiles serverList)

getFile :: DirectoryServer -> String -> Handler (Maybe File)
getFile ds@DirectoryServer{..} file = do
  let fHash = hash file
  ftl <- liftIO $ atomically $ readTVar fileToLocations
  fileSearch fHash ftl
  
fileSearch :: Int -> Map Int [FS.FileServer] -> Handler (Maybe File)
fileSearch h ftl = case Map.lookup h ftl of
                    Nothing     -> return Nothing
                    Just server -> do
   	                 try server h
                     where
                      try :: [FS.FileServer] -> Int -> Handler (Maybe File)
                      try []     _ = return Nothing
                      try (s:ss) h = case liftIO (FS.getFile s h) of
                       Nothing -> try ss h
                       Just f  -> return f

runServer :: DirectoryServer -> Handle -> IO ()
runServer ds@DirectoryServer{..} hdl = do
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
      content <- hGetLine hdl
      putStrLn $ "\t>" ++ newfilename ++ " to be created. Contents: " ++ content
      hPutStrLn hdl "wow demanding arent u"
      interact

     ["UPDATE_FILE:", filename] -> do
      putStrLn $ "\t>" ++ filename ++ " to be updated"
      hPutStrLn hdl "i like it how it is"
      interact

     ["QUIT"] -> putStrLn "lol weak ass kid" >> hPutStrLn hdl "lol bye bish"

     _ -> putStrLn "wildcard" >> interact