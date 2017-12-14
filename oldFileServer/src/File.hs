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

module File ( File(..) , newFile , getPath) where

import qualified Locking as L
import System.FilePath
import Data.Aeson.Types
import GHC.Generics
import Control.Concurrent.STM
import Control.Monad.IO.Class

data File = File
  { path    :: FilePath
  , state   :: L.FileState
  } deriving Generic

instance ToJSON File

instance Show File where
  show f@File{..} = "[" ++ show path ++ "]"

newFile :: FilePath -> File
newFile loc = File { path    = loc
                   , state   = L.available
                   }

getContent :: FilePath -> IO String
getContent = readFile

getPath :: File -> String
getPath f@File{..} = path