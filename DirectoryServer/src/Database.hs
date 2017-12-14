{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE RecordWildCards            #-}

module Database (runDatabase) where

import Yesod hiding (fileName)

import Database.Persist.Sqlite
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)

import Types
--import Config

-- Define our entities as usual
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
FileServer
    port Int
    accessCount Int default=0
    UniquePort port
    deriving Show
File
    name String
    accessCount Int default=0
    hostServerId FileServerId
    UniqueLoc name hostServerId
    deriving Show
|]

-- We keep our connection pool in the foundation. At program initialization, we
-- create our initial pool, and each time we need to perform an action we check
-- out a single connection from the pool.
data PersistTest = PersistTest ConnectionPool

-- We'll create a single route, to access a person. It's a very common
-- occurrence to use an Id type in routes.
mkYesod "PersistTest" [parseRoutes|
/ HomeR GET
/server/#FileServerId ServerR GET
/file/#FileId FileR GET
|]

-- Nothing special here
instance Yesod PersistTest

-- Now we need to define a YesodPersist instance, which will keep track of
-- which backend we're using and how to run an action.
instance YesodPersist PersistTest where
    type YesodPersistBackend PersistTest = SqlBackend

    runDB action = do
        PersistTest pool <- getYesod
        runSqlPool action pool

--Example db functions w/ rendering
-- List all file servers in the database
getHomeR :: Handler Html
getHomeR = do
    servers <- runDB $ selectList [] [Asc FileServerAccessCount]
    defaultLayout $ do
        setTitle "Available Servers"
        [whamlet|
            <ul>
                $forall Entity serverid server <- servers
                    <li>
                        <a href=@{ServerR serverid}> Server at #{fileServerPort server}
        |]

getServerR :: FileServerId -> Handler Html
getServerR fileServerId = do
    files <- runDB $ selectList [FileHostServerId ==. fileServerId] []
    defaultLayout
        [whamlet|
            <ul>
                $forall Entity fileid file <- files
                    <li>
                        <a href=@{FileR fileid}>#{fileName file}
        |]


getFileR :: FileId -> Handler String
getFileR fileId = do
    file <- runDB $ get404 fileId
    return $ show file
{-
getFilesR :: FileServerId -> Handler Html
getFilesR fileServerId = do
  filelocations <- runDB $ selectList [ServerId ==. fileServerId] []
  defaultLayout
      [whamlet|
          <ul>
              $forall Entity filelocationid filelocation <- filelocations
                  <li>
                      (Y)
      |]

-- We'll just return the show value of a person, or a 404 if the Person doesn't
-- exist.
getCommitR :: CommitId -> Handler String
getCommitR commitId = do
    commit <- runDB $ get404 commitId
    return $ show commit
-}

openConnectionCount :: Int
openConnectionCount = 10

runDatabase :: IO ()
runDatabase = runStderrLoggingT $ withSqlitePool "tester1.db3" openConnectionCount $ \pool -> liftIO $ do
    runResourceT $ flip runSqlPool pool $ do
        runMigration migrateAll
        server <- insert $ FileServer 1234 0
        insert $ File "The meaning of life" 0 server
    warp 3000 $ PersistTest pool