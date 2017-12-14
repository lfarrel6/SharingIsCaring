{-# LANGUAGE RecordWildCards #-}

module Types (File) where

data File = File {
  name    :: String
, content :: String
}