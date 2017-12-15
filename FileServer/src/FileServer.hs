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

import MyApi.DirectoryServerApi (addServer')
import MyApi.FileServerApi
import MyApi.FileApi
import MyApi.Query

import Config

data FileServer = FileServer
  { directoryAddr :: FilePath
  , fileListing   :: TVar (Map Int File) --mapping of hashes to files
  , port          :: Int
  , fileCount     :: TVar Int
  , mothership    :: Int --port of mothership (directory server)
  }

instance Show FileServer where
  show fs@FileServer{..} = "[FileServer: " ++ show port ++ "]\n\t>[Port: "++ show port ++"]\n\t>[Directory: "++show directoryAddr ++ "]"

getPort fs@FileServer{..} = port

buildFileServer :: Config -> IO ()
buildFileServer cfg@Config{..} = do
  fs <- liftIO $ initialiseFileServer fileServerPort directoryServerPort
  --query to directory server to let know file server is alive
  files <- liftIO $ getAllFiles fs
  liftIO $ query (addServer' fileServerPort files) ("localhost",directoryServerPort)
  startServer fs

getAllFiles :: FileServer -> IO [File]
getAllFiles fs@FileServer{..} = do
 filesMap <- atomically $ readTVar fileListing
 return $ Map.elems filesMap

initialiseFileServer :: Int -> Int -> IO FileServer
initialiseFileServer portNum ds = do
  fs <- newFileServer portNum ds
  let dp = directoryAddr fs
  createDirectoryIfMissing True dp
  creationTime <- fmap show getCurrentTime
  writeFile (dp++"/serverDOB.txt") creationTime
  let starterFile = makeFile "serverDOB.txt" creationTime (dp++"/serverDOB.txt")
  files <- listDirectory dp
  newFiles <- newFile files []
  let contents = initFileList newFiles 
  let newFileCount = length files
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
newFileServer portNum ds = do
  let dirPath = "data/" ++ show portNum
  fl    <- atomically $ getFileList
  fc    <- atomically $ getFileCount
  return FileServer { directoryAddr = dirPath
                    , fileListing   = fl
                    , port          = portNum
                    , fileCount     = fc
                    , mothership    = ds
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

fsApp :: FileServer -> Application
fsApp fs = serve fileServerAPI $ myFileServer fs

startServer :: FileServer -> IO ()
startServer fs@FileServer{..} = run port $ fsApp fs

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