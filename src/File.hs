{-# LANGUAGE RecordWildCards #-}

module File ( File(..) , newFile , getPath) where

import qualified Locking as L
import System.FilePath

data File = File
  { path  :: FilePath
  , state :: L.FileState
  }

instance Show File where
  show f@File{..} = "File at: " ++ show path

newFile :: String -> File
newFile loc = File { path  = loc
                   , state = L.available
                   }

getPath :: File -> String
getPath f@File{..} = path