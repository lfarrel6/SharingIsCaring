{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api (app) where

import           Control.Monad.Except
import           Data.Int             (Int64)
import           Servant              ((:<|>) ((:<|>)), (:~>) (NT),
                                       Proxy (Proxy), Raw, ServantErr, Server,
                                       enter, serve, serveDirectoryFileServer)
import           Servant.Server

import           Api.User             (UserAPI, userServer)
import           Config               (AppT (..), Config (..))
import           Control.Category     ((<<<), (>>>))

{- This is the function that will give access to the application -}
userApp :: Config -> Application
userApp cfg = serve (Proxy :: Proxy UserAPI) (appToServer cfg)

{- Use arrows to transform the app to the IO monad, converts app to runable server -}
appToServer :: Config -> Server UserAPI
appToServer cfg = enter (convertApp cfg >>> NT Handler) userServer

{- Convert from AppT to ExceptT ServantErr -> making it servant compatible -}
convertApp :: Config -> AppT m :~> ExceptT ServantErr m
convertApp cfg = runReaderTNat cfg <<< NT runApp

{- load in any frontend files and serve using WAI -}
files :: Server Raw
files = serveDirectoryFileServer "assets"

{- Define the API, FileAPI defined separately for readability, attach Raw as wildcard -}
type AppAPI = FileAPI :<|> Raw

type FileAPI = "file" :> Capture "fileId" Integer :> Get '[JSON] File
          :<|> "file" :> Capture "fileId" Integer :> "edit" :> Get '[JSON] File  

appApi :: Proxy AppAPI
appApi = Proxy

{- Serve the application, files rendered at Raw endpoint -}
app :: Config -> Application
app cfg =
    serve appApi (appToServer cfg :<|> files)