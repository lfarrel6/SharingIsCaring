{-# LANGUAGE RecordWildCards #-}

module Client () where

import DirectoryServer

getFile :: DirectoryServer -> String -> Handler (Maybe File)
getFile ds@DirectoryServer{..} f = --send message to get file

getFiles :: DirectoryServer -> Handler [File]
getFiles ds = --send message to get all files froim a given server

