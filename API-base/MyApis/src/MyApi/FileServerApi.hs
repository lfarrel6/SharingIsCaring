{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module MyApi.FileServerApi (getFile' , getFiles' , fileServerAPI , FileServerAPI) where

import Network.HTTP.Client (newManager, defaultManagerSettings)
import Data.Aeson
import Data.Proxy
import Servant.API
import Servant.Client
import GHC.Generics

import MyApi.FileApi

type FileServerAPI = "files" :> Get '[JSON] [File]
                :<|> "file"  :> Capture "name" String :> Get '[JSON] (Maybe File)
          --    :<|> "update":> QueryParam "pathToFile" String :> Post '[JSON] File

fileServerAPI :: Proxy FileServerAPI
fileServerAPI = Proxy

api :: Proxy FileServerAPI
api = Proxy

--updateFile' :: Maybe String -> ClientM File
getFiles'   :: ClientM [File]
getFile'    :: String -> ClientM (Maybe File)
getFiles' :<|> getFile' = client fileServerAPI