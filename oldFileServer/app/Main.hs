{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import MyApi.DirectoryServerApi
import MyApi.FileApi
import DirectoryServer
import Config
import System.Environment

import Control.Concurrent.STM
import Prelude ()
import Prelude.Compat
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson.Compat
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.API
import Servant.Client
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html

main :: IO ()
main = do
  received <- getConfig
  case received of
   Just cfg -> do
    putStrLn "Successful invocation"
    ds <- newDirectory
    startServer ds (directoryPort cfg) (startPort cfg) (nServers cfg)
   Nothing  -> putStrLn "Bad Parameters."

{-
type FileAPI = "files" :> Get '[JSON] [File]
          :<|> "file"  :> Capture "name" String :> Get '[JSON] (Maybe File)
          :<|> "create":> Post '[JSON] File
          :<|> "update":> Capture "name" String :> Put '[JSON] File

myServer :: DirectoryServer -> Server FileAPI
myServer ds = getFiles ds
         :<|> getFile ds  
         :<|> sendFile ds
         :<|> updateFile ds

getFiles :: DirectoryServer -> Handler [File]
getFiles ds = do
  resp <- liftIO $ query getFiles' ("localhost",1235)
  case resp of
   Right fs -> return fs
   _        -> return $ [newFile ".."]
-- getFiles _ = return $ [newFile ""]

getFile :: DirectoryServer -> String -> Handler (Maybe File)
getFile ds f = getFile ds f

sendFile :: DirectoryServer -> Handler File
sendFile _ = return $ newFile ""

updateFile :: DirectoryServer -> String -> Handler File
updateFile _ s = return $ newFile s

fAPI :: Proxy FileAPI
fAPI = Proxy

myApp :: DirectoryServer -> Application
myApp ds = serve fAPI $ myServer ds

main :: IO ()
main = do
  ds <- newDirectory
  startServer ds 1235
  run 1234 $ myApp ds
-}