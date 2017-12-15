{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module MyApi.Query (query, simpleQuery, queryNStrip) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant.API
import Servant.Client
import Network.HTTP.Client (newManager, defaultManagerSettings)


query req (host,port) = do
  newDefManager <- newManager defaultManagerSettings
  runClientM req (ClientEnv newDefManager (BaseUrl Http host port ""))

simpleQuery req (host,port) = do
  response <- query req (host,port)
  case response of
   Right _ -> return True
   Left  _ -> return False

queryNStrip req (host,port) = do
  response <- query req (host,port)
  case response of
   Right x -> return x
   Left  y -> return y