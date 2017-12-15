{-# LANGUAGE RecordWildCards #-}

module FileServer ( FileServer , getPort, startServer , buildFileServer , getFiles , getFile ) where

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
import Network.Wai
import Network.Wai.Handler.Warp hiding (getPort)

import MyApi.FileServerApi
import MyApi.FileApi
import MyApi.Query

import Message

data FileServer = FileServer
  { serverID      :: Int
  , directoryAddr :: FilePath
  , fileListing   :: TVar (Map Int File) --mapping of hashes to files
  , port          :: Int
  , fileCount     :: TVar Int
  , msgChan       :: TChan Message
  }

instance Show FileServer where
  show fs@FileServer{..} = "[FileServer: " ++ show serverID ++ "]\n\t>[Port: "++ show port ++"]\n\t>[Directory: "++show directoryAddr ++ "]"

getPort fs@FileServer{..} = port

buildFileServer :: Int -> Int -> IO FileServer
buildFileServer id portNum = do
  fs <- newFileServer id portNum
  let dp = directoryAddr fs
  createDirectoryIfMissing True dp
  creationTime <- fmap show getCurrentTime
  writeFile (dp++"/serverDOB.txt") creationTime
  let starterFile = makeFile (dp++"/serverDOB.txt") creationTime "serverDOB.txt"
  --files <- listDirectory dp
  --newFiles <- newFile files []
  let contents = initFileList [starterFile]--newFiles 
  let newFileCount = 1 --length files
  atomically $ writeTVar (fileListing fs) contents
  atomically $ writeTVar (fileCount   fs) newFileCount
  return fs

newFile :: [FilePath] -> [File] -> IO [File]
newFile [] fs = return fs
newFile (fp:fps) fs = do
  content <- readFile fp
  let file = makeFile fp content fp
  newFile fps (fs ++ [file])

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

initFileList :: [File] -> Map Int File
initFileList []     = Map.empty
initFileList (f:fs) = Map.insert (hash $ getLocation f) f $ initFileList fs

addFile :: FileServer -> File -> IO ()
addFile fs@FileServer{..} f = do
  -- insert file to server
  insertFile
  --putStrLn ("[File " ++ show path ++ " - " ++ show state ++ "] added to [FileServer " ++ show serverID ++ "]")
  where
   insertFile = atomically $ do
    files <- readTVar fileListing
    let fID = hash $ getLocation f
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
startServer fs@FileServer{..} = run port $ fsApp fs

handleMsg :: FileServer -> Message -> IO Bool
handleMsg fs@FileServer{..} msg = return True

{- \\\ SERVANT /// -}

myFileServer :: FileServer -> Server FileServerAPI
myFileServer fs = getFiles fs
             :<|> getFile  fs  
         -- :<|> sendFile fs

getFiles :: FileServer -> Handler [File]
getFiles fs@FileServer{..} = do
  files <- liftIO $ atomically $ readTVar fileListing
  return $ Map.elems files

getFile :: FileServer -> String -> Handler (Maybe File)
getFile fs@FileServer{..} requested = do
 files <- liftIO $ atomically $ readTVar fileListing
 return $ Map.lookup (hash requested) files

{-  
  let fHash = hash file
  ftl <- liftIO $ atomically $ readTVar fileToLocations
  fileSearch fHash ftl

sendFile :: DirectoryServer -> File -> Handler Bool
sendFile ds@DirectoryServer{..} f = do
  res <- liftIO $ query (sendFile' f) ("localhost",1235)
  case res of
   Right x -> return x
   Left  _ -> return False
  --invoke method on fileserver
  -- return $ F.newFile ".."
-}
fsApp :: FileServer -> Application
fsApp fs = serve fileServerAPI $ myFileServer fs