{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Main
  ( main
  ) where

import Control.Monad.Trans.Reader
import Network.Wai.Handler.Warp
import Servant
import Servant.API.Generic (Generic)
import qualified Reader as R
import Control.Monad.Trans.Class (MonadTrans(lift))
import Database.PostgreSQL.Simple (connectPostgreSQL)
import Routes.Handler
import Routes.Types

type API
   = NamedRoutes Routes :<|> "version" :> Get '[ JSON] String :<|> "healthcheck" :> Get '[ JSON] String

data Routes mode =
  Routes
    { _getAllBooks :: mode :- "books" :> Get '[ JSON] [Book]
    , _getBook :: mode :- "books" :> Capture "id" String :> Get '[ JSON] Book
    , _postBook :: mode :- "books" :> ReqBody '[ JSON] Book :> Post '[ JSON] Book
    , _deleteBook :: mode :- "books" :> Capture "id" String :> Delete '[ JSON] NoContent
    }
  deriving (Generic)

main :: IO ()
main = do
  conn <- connectPostgreSQL "host=localhost port=5432 dbname=bookstore user=bilbo password=baggins"
  let runApp = runReaderT startApp :: R.Env -> IO ()
      env =
        R.Env
          { sqlConn = conn
          }
  runApp env

startApp :: R.ReaderIO ()
startApp = do
  env <- ask
  lift $ run 8081 (app env)

app :: R.Env -> Application
app env = serve (Proxy @API) server
  where
    server =
      let getAllBooksHandler = runReaderT getAllBooks env
          getBookHandler i = runReaderT (getBook i) env
          postBookHandler i = runReaderT (postBook i) env
          deleteBookHandler i = runReaderT (deleteBook i) env
      in
      Routes
        { _getAllBooks = getAllBooksHandler
        , _getBook = getBookHandler
        , _postBook = postBookHandler
        , _deleteBook = deleteBookHandler
        } :<|>
      return "1.0.0" :<|>
      return "App is UP"

