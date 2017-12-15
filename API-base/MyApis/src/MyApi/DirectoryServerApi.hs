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

module MyApi.DirectoryServerApi (getFiles', getFile' , sendFile' , dsAPI , QuerySuccess(..) , DirectoryAPI) where

import Network.HTTP.Client (newManager, defaultManagerSettings)
import Data.Aeson
import Data.Proxy
import Servant.API
import Servant.Client
import GHC.Generics

import MyApi.FileApi (File)

type DirectoryAPI =  "files" :> Get '[JSON] [File]
                :<|> "file"  :> Capture "name" String :> Get '[JSON] (Maybe File)
                :<|> "create":> ReqBody '[JSON] File :> Post '[JSON] Bool

dsAPI :: Proxy DirectoryAPI
dsAPI = Proxy

newtype QuerySuccess = QuerySuccess {

  result :: Bool

} deriving (Generic,Show)
instance ToJSON QuerySuccess
instance FromJSON QuerySuccess

api :: Proxy DirectoryAPI
api = Proxy

getFiles' :: ClientM [File]
getFile' :: String -> ClientM (Maybe File)
sendFile' :: File -> ClientM Bool
getFiles' :<|> getFile' :<|> sendFile' = client dsAPI