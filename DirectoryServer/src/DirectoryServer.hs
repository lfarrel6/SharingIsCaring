{-# LANGUAGE RecordWildCards #-}

module DirectoryServer
    ( DirectoryServer , startServer , newDirectory , getAllFiles ) where

import qualified FileServer as FS
import Message

import MyApi.DirectoryServerApi 
import qualified MyApi.FileServerApi as FSA
import MyApi.FileApi (File)
import MyApi.Query

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
import Network.Wai
import Network.Wai.Handler.Warp

someFunc :: IO ()
someFunc = do
  newDS <- newDirectory
  showDS newDS

data DirectoryServer = DirectoryServer
  { fileToLocations :: TVar (Map Int [Int]) --Hash of filenames to port numbers
  , allServers      :: TVar (Map Int Int)   --Server IDs to Port Numbers
  , nServers        :: TVar Int             --Number of live File Servers
  , nextID          :: TVar Int             --Next server ID
  }

newDirectory :: IO DirectoryServer
newDirectory = do
  ftl   <- atomically $ newTVar Map.empty
  as    <- atomically $ newTVar Map.empty
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
startServer ds dsPort fsPort nFs = do
  initFileServers ds fsPort nFs
  run dsPort $ dsApp ds

initFileServers :: DirectoryServer -> Int -> Int -> IO ()
initFileServers _ _ 0          = putStrLn "File Servers created"
initFileServers ds startPort n = createFileServer ds startPort >> initFileServers ds (startPort+1) (n-1)

{- \\\ SERVANT /// -}

myDirServer :: DirectoryServer -> Server DirectoryAPI
myDirServer ds = getAllFiles ds
            :<|> getFile ds  
            :<|> sendFile ds

getAllFiles :: DirectoryServer -> Handler [File]
getAllFiles ds@DirectoryServer{..} = do
  servers <- liftIO $ atomically $ readTVar allServers
  let serverList = Map.elems servers
  getAllServerFiles [] serverList
  --files <- liftIO $ query getFiles' ("localhost",1235)
  where
   getAllServerFiles :: [File] -> [FS.FileServer] -> Handler [File]
   getAllServerFiles r []   = return r
   getAllServerFiles r (x:xs) = do
    let port = FS.getPort x
    theseFiles <- liftIO $ query FSA.getFiles' ("localhost",port)
    case theseFiles of
     Right fs -> getAllServerFiles (r ++ fs) xs
     Left  _  -> getAllServerFiles r xs

getFile :: DirectoryServer -> String -> Handler (Maybe File)
getFile ds@DirectoryServer{..} file = do
  file <- liftIO $ query (getFile' file) ("localhost",1235)
  case file of
   Right x -> return x
   Left  _ -> return Nothing

sendFile :: DirectoryServer -> File -> Handler Bool
sendFile ds@DirectoryServer{..} f = do
  res <- liftIO $ query (sendFile' f) ("localhost",1235)
  case res of
   Right x -> return x
   Left  _ -> return False

dsApp :: DirectoryServer -> Application
dsApp ds = serve dsAPI $ myDirServer ds