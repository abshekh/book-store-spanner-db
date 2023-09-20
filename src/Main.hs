{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main
  ( main,
  )
where

import Control.Exception (try)
import Control.Monad.Except
import Control.Monad.Trans.Reader
import Database.PostgreSQL.Simple
import Network.Wai.Handler.Warp
import qualified Reader as R
import Routes.Handler
import Routes.Types
import Servant
import System.Environment
import Network.Google.Resource.Spanner.Projects.Instances.Databases.Sessions.ExecuteSQL (projectsInstancesDatabasesSessionsExecuteSQL)
import Network.Google.Spanner (executeSQLRequest)
import Network.Google as Google
import Control.Monad.Trans.Resource (liftResourceT, runResourceT)
import System.IO (stdout)

type API =
  "books"
    :> ( Get '[JSON] [Book]
           :<|> Capture "id" String :> Get '[JSON] Book
           :<|> ReqBody '[JSON] Book :> Post '[JSON] Book
           :<|> Capture "id" String :> Delete '[JSON] NoContent
       )
    :<|> "version" :> Get '[JSON] String
    :<|> "healthcheck" :> Get '[JSON] String

main :: IO ()
main = do
  dbHost <- getEnv "dbHost"
  dbPort <- getEnv "dbPort"
  dbName <- getEnv "dbName"
  dbUser <- getEnv "dbUser"
  dbPassword <- getEnv "dbPassword"
  conn <-
    connect $
      ConnectInfo
        { connectUser = dbUser,
          connectPort = read dbPort,
          connectPassword = dbPassword,
          connectHost = dbHost,
          connectDatabase = dbName
        }
  let env =
        R.Env
          { sqlConn = conn
          }
  let e = executeSQLRequest
  let p = projectsInstancesDatabasesSessionsExecuteSQL e "select * from books"

  lgr <- Google.newLogger Google.Debug stdout
  env <-
    Google.newEnv
      <&> (Google.envLogger .~ lgr)
        . (Google.envScopes .~ Storage.storageReadWriteScope)
  runResourceT 
  let r = Google.send p
  run 8081 (app env)

app :: R.Env -> Application
app env = serve (Proxy @API) server
  where
    server =
      let getAllBooksHandler = Handler $ ExceptT $ try $ runReaderT getAllBooks env
          getBookHandler i = Handler $ ExceptT $ try $ runReaderT (getBook i) env
          postBookHandler i = Handler $ ExceptT $ try $ runReaderT (postBook i) env
          deleteBookHandler i = Handler $ ExceptT $ try $ runReaderT (deleteBook i) env
       in ( getAllBooksHandler
              :<|> getBookHandler
              :<|> postBookHandler
              :<|> deleteBookHandler
          )
            :<|> return "1.0.0"
            :<|> return "App is UP"
