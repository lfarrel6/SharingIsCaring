{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Config where

import           Control.Exception                    (throwIO)
import           Control.Monad.Except                 (ExceptT, MonadError)
import           Control.Monad.IO.Class
import           Control.Monad.Logger                 (runStderrLoggingT)
import           Control.Monad.Reader                 (MonadIO, MonadReader,
                                                       ReaderT, ask, asks)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe            (MaybeT (..), runMaybeT)
import qualified Data.ByteString.Char8                as BS
import           Data.Monoid                          ((<>))
import           Data.Text
import           Database.Persist.Sqlite              

import           Servant                              (ServantErr)
import           System.Environment                   (lookupEnv)

{- Monad Stack: Servant monad wrapped using ReaderT Config providing the -}
{- MonadReader's ask ability to interrogate the config when necessary    -}
newtype AppT m a
    = AppT
    { runApp :: ReaderT Config (ExceptT ServantErr m) a
    } deriving ( Functor, Applicative, Monad, MonadReader Config,
                 MonadError ServantErr, MonadIO)

type App = AppT IO

{- The config holds the persistent connection pool, environment          -}
data Config
    = Config
    { configPool    :: ConnectionPool
    , configEnv     :: Environment
    }

{-
-- | MonadLogger instance to use within @AppT m@
instance MonadIO m => MonadLogger (AppT m) where
    monadLoggerLog = adapt logMsg

-- | MonadLogger instance to use in @makePool@
instance MonadIO m => MonadLogger (KatipT m) where
    monadLoggerLog = adapt logMsg
-}

{- Data constructor for the possible environments: Development/Testing   -}
data Environment
    = Development
    | Test
    deriving (Eq, Show, Read)

{- Make Connection pool for Testing and Dev environments                  -}
makePool :: Environment -> IO ConnectionPool
makePool Test = runStderrLoggingT $ createSqlitePool (connStr "test") (openConnectionCount Test)
makePool Development = runStderrLoggingT $ createSqlitePool (connStr "dev") (openConnectionCount Development)

{- Default value for connection count, would need other options for prod -}
openConnectionCount :: Environment -> Int
openConnectionCount _ = 10

{- Get connection string, prepend argument to string to separate Test&Dev-}
connStr :: Text -> Text
connStr pre = pre <> ".fileserver"