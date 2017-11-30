{-# LANGUAGE RecordWildCards #-}

module FileServer ( FileServer , newFileServer , initFileList , addFile ) where

import qualified File as F
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Concurrent.STM
import Data.Hashable
import Network

data FileServer = FileServer
  { serverID      :: Int
  , directoryAddr :: FilePath
  , fileListing   :: TVar (Map Int F.File)
  , port          :: PortNumber
  }

instance Show FileServer where
  show fs@FileServer{..} = "[ServerID: " ++ show serverID ++ "]"

newFileServer :: Int -> FilePath -> [F.File] -> PortNumber -> STM FileServer
newFileServer id dir files portNum = do
  fileList <- newTVar $ initFileList files
  return FileServer { serverID      = id
                    , directoryAddr = dir
                    , fileListing   = fileList
                    , port          = portNum
                    }

initFileList :: [F.File] -> Map Int F.File
initFileList files = init' files

init' :: [F.File] -> Map Int F.File
init' []     = Map.empty
init' (x:xs) = Map.insert (hash $ F.getPath x) x $ init' xs

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