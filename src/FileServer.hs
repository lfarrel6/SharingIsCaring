{-# LANGUAGE RecordWildCards #-}

module FileServer ( FileServer , buildFileServer , initFileList , addFile , startServer , getFiles , getFile ) where

import qualified Data.Map as Map
import Data.Map (Map)
import Control.Concurrent.STM
import Data.Hashable
import Network
import System.IO
import System.Directory
import Data.Time
import Control.Monad
import Control.Monad.IO.Class
import Servant

import qualified File as F
import Message

data FileServer = FileServer
  { serverID      :: Int
  , directoryAddr :: FilePath
  , fileListing   :: TVar (Map Int F.File) --mapping of hashes to files
  , port          :: Int
  , fileCount     :: TVar Int
  , msgChan       :: TChan Message
  }

instance Show FileServer where
  show fs@FileServer{..} = "[FileServer: " ++ show serverID ++ "]\n\t>[Port: "++ show port ++"]\n\t>[Directory: "++show directoryAddr ++ "]"

buildFileServer :: Int -> Int -> IO FileServer
buildFileServer id portNum = do
  fs <- newFileServer id portNum
  let dp = directoryAddr fs
  createDirectoryIfMissing True dp
  creationTime <- fmap show getCurrentTime
  writeFile (dp++"/serverDOB.txt") creationTime
  files <- listDirectory dp
  let contents = initFileList $ map F.newFile files
  let newFileCount = length files
  atomically $ writeTVar (fileListing fs) contents
  atomically $ writeTVar (fileCount   fs) newFileCount
  return fs

newFileServer :: Int -> Int -> IO FileServer
newFileServer id portNum = do
  let dirPath = "data/" ++ show id
  fl    <- atomically $ getFileList
  fc    <- atomically $ getFileCount
  mchan <- newTChanIO
  return FileServer { serverID      = id
                    , directoryAddr = dirPath
                    , fileListing   = fl
                    , port          = portNum
                    , fileCount     = fc
                    , msgChan       = mchan
                    }
  where
   getFileList  = newTVar Map.empty
   getFileCount = newTVar 0

initFileList :: [F.File] -> Map Int F.File
initFileList []                = Map.empty
initFileList (f@F.File{..}:fs) = Map.insert (hash path) f $ initFileList fs

addFile :: FileServer -> F.File -> IO ()
addFile fs@FileServer{..} f@F.File{..} = do
  -- insert file to server
  insertFile
  putStrLn ("[File " ++ show path ++ " - " ++ show state ++ "] added to [FileServer " ++ show serverID ++ "]")
  where
   insertFile = atomically $ do
    files <- readTVar fileListing
    let fID = hash path
    case Map.lookup fID files of
     Nothing -> do
      --insert
      let newListing = Map.insert fID f files
      writeTVar fileListing newListing
     Just _  -> do
      --update file
      --naive update: assumes given file is more recent than held file - needs to change
      let fileRemoved = Map.delete fID files
          newListing  = Map.insert fID f fileRemoved
      writeTVar fileListing newListing

startServer :: FileServer -> IO ()
startServer fs@FileServer{..} = withSocketsDo $ do
  sock <- listenOn $ portNum port
  print fs
  --listen
  where
   portNum n = PortNumber $ fromIntegral n
   listen = join $ atomically $ do
    msg <- readTChan msgChan
    return $ do
      continue <- handleMsg fs msg
      when continue listen
    --hPutStrLn handle $ "You have reached [File Server " ++ show serverID ++ "]"
    --listen s

handleMsg :: FileServer -> Message -> IO Bool
handleMsg fs@FileServer{..} msg = return True

getFiles :: FileServer -> Handler [F.File]
getFiles fs@FileServer{..} = do
  files <- liftIO $ atomically $ readTVar fileListing
  return $ Map.elems files

getFile :: FileServer -> Int -> IO (Maybe F.File)
getFile fs@FileServer{..} requested = do
 files <- atomically $ readTVar fileListing
 return $ Map.lookup requested files