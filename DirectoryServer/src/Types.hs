{-# LANGUAGE RecordWildCards #-}

module Types (File(..)) where

import Data.Text

data File = File
   { name    :: Text
   , content :: Text
   }
  deriving Show