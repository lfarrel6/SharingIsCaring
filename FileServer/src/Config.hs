module Config (Config(..), parseConfig) where

data Config  = Config {
  fileServerPort      :: Int
, directoryServerPort :: Int
}

parseConfig :: [String] -> IO (Maybe Config)
parseConfig args = case args of
  [fsp,dsp] -> return $ Just $ Config{ fileServerPort = (read fsp :: Int), directoryServerPort = (read dsp :: Int) }
  _         -> return Nothing