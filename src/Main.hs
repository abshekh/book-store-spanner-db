{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Servant.API.Generic
import System.Environment

type API =
  NamedRoutes Routes :<|> "version" :> Get '[JSON] String :<|> "healthcheck" :> Get '[JSON] String

data Routes mode = Routes
  { _getAllBooks :: mode :- "books" :> Get '[JSON] [Book],
    _getBook :: mode :- "books" :> Capture "id" String :> Get '[JSON] Book,
    _postBook :: mode :- "books" :> ReqBody '[JSON] Book :> Post '[JSON] Book,
    _deleteBook :: mode :- "books" :> Capture "id" String :> Delete '[JSON] NoContent
  }
  deriving (Generic)

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
  run 8081 (app env)

app :: R.Env -> Application
app env = serve (Proxy @API) server
  where
    server :: Server API
    server =
      let getAllBooksHandler = Handler $ ExceptT $ try $ runReaderT getAllBooks env
          getBookHandler i = Handler $ ExceptT $ try $ runReaderT (getBook i) env
          postBookHandler i = Handler $ ExceptT $ try $ runReaderT (postBook i) env
          deleteBookHandler i = Handler $ ExceptT $ try $ runReaderT (deleteBook i) env
       in Routes
            { _getAllBooks = getAllBooksHandler,
              _getBook = getBookHandler,
              _postBook = postBookHandler,
              _deleteBook = deleteBookHandler
            }
            :<|> return "1.0.0"
            :<|> return "App is UP"
