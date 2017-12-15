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

module MyApi.FileApi (readFile' , createFile' , updateFile' , makeFile , getLocation , fileAPI , FileAPI, File) where

import Network.HTTP.Client (newManager, defaultManagerSettings)
import Data.Aeson
import Data.Proxy
import Servant.API
import Servant.Client
import GHC.Generics

data File = File {
  name     :: String,
  content  :: String,
  location :: String
} deriving (Show,Generic)
instance FromJSON File
instance ToJSON File

makeFile :: String -> String -> String -> File
makeFile n c l = File{
  name     = n
, content  = c
, location = l
}
getLocation f@File{..} = location

newtype QuerySuccess = QuerySuccess {

  result :: Bool

} deriving (Generic,Show)
instance ToJSON QuerySuccess
instance FromJSON QuerySuccess

type FileAPI = "read"  :> QueryParam "pathToFile" String :> Get '[JSON] (Maybe File)
          :<|> "create":> ReqBody '[JSON] File           :> Post '[JSON] QuerySuccess
          :<|> "update":> QueryParam "pathToFile" String :> Post '[JSON] File

fileAPI :: Proxy FileAPI
fileAPI = Proxy

api :: Proxy FileAPI
api = Proxy

readFile'   :: Maybe String -> ClientM (Maybe File)
createFile' :: File -> ClientM QuerySuccess
updateFile' :: Maybe String -> ClientM File
readFile' :<|> createFile' :<|> updateFile' = client fileAPI