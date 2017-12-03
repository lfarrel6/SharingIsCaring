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

import DirectoryServer
import Config
import File
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
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html


type FileAPI = "files" :> Get '[JSON] [File]
          :<|> "file"  :> Capture "name" String :> Get '[JSON] (Maybe File)

getFile :: DirectoryServer -> String -> Handler (Maybe File)
getFile ds f = getFile ds f

getFiles :: DirectoryServer -> Handler [File]
getFiles = getAllFiles

myServer :: DirectoryServer -> Server FileAPI
myServer ds = getFiles ds
         :<|> getFile ds  

fAPI :: Proxy FileAPI
fAPI = Proxy

myApp :: DirectoryServer -> Application
myApp ds = serve fAPI $ myServer ds

main :: IO ()
main = do
  ds <- newDirectory
  startServer ds 1235 1235 7
  run 1234 $ myApp ds

{-main :: IO ()
main = do
  received <- getConfig
  case received of
   Just cfg -> do
    --putStrLn "Successful invocation"
    ds <- newDirectory
    startServer ds (directoryPort cfg) (startPort cfg) (nServers cfg)
   Nothing  -> putStrLn "Bad Parameters."
   -}