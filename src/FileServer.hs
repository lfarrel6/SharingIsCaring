{-# LANGUAGE RecordWildCards #-}

module FileServer ( FileServer , buildFileServer , initFileList , addFile , startServer ) where

import qualified File as F
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Concurrent.STM
import Data.Hashable
import Network
import System.IO
import System.Directory
import Data.Time

data FileServer = FileServer
  { serverID      :: Int
  , directoryAddr :: FilePath
  , fileListing   :: TVar (Map Int F.File)
  , port          :: Int
  }

instance Show FileServer where
  show fs@FileServer{..} = "[ServerID: " ++ show serverID ++ "]"

buildFileServer :: Int -> Int -> IO FileServer
buildFileServer id portNum = do
  fs <- atomically $ newFileServer id portNum
  let dp = (directoryAddr fs)
  createDirectoryIfMissing True dp
  creationTime <- (fmap show getCurrentTime)
  writeFile (dp++"/serverDOB.txt") creationTime
  files <- listDirectory dp
  let contents = initFileList $ map F.newFile files
  atomically $ writeTVar (fileListing fs) contents
  return fs

newFileServer :: Int -> Int -> STM FileServer
newFileServer id portNum = do
  let dirPath = ("data/" ++ show id)
  fl <- newTVar Map.empty
  return FileServer { serverID      = id
                    , directoryAddr = dirPath
                    , fileListing   = fl
                    , port          = portNum
                    }

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
  putStrLn $ "\t>[FileServer " ++ show serverID ++ "] listening on " ++ show port
  fMap <- atomically $ readTVar fileListing
  let files = Map.elems fMap
  mapM_ (\x -> putStrLn $ show x) files
  listen sock
  where
   portNum n = PortNumber $ fromIntegral n
   listen s = do
    (handle, host, _) <- accept s
    hPutStrLn handle $ "You have reached [File Server " ++ show serverID ++ "]"
    listen s