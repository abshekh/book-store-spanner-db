module Reader where

import qualified Control.Monad.Catch as C
import Control.Monad.Trans.Reader
import qualified Data.ByteString.Lazy as BL
import Data.Text
import qualified Data.Text.Encoding as BL
import Database.PostgreSQL.Simple
import qualified Servant as S

type ReaderH = ReaderT Env S.Handler

type ReaderIO = ReaderT Env IO

newtype Env = Env
  { sqlConn :: Connection
  }

getSqlConnection :: ReaderH Connection
getSqlConnection = do
  Env {..} <- ask
  return sqlConn

data ApiError = ApiError S.ServerError Text

throwApi :: ApiError -> ReaderH a
throwApi = C.throwM . toServantError
  where
    toServantError (ApiError errCode err) = errCode {S.errBody = BL.fromStrict $ BL.encodeUtf8 err}
