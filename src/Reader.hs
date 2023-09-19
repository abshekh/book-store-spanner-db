module Reader where

import qualified Control.Monad.Catch as C
import Control.Monad.Trans.Reader
import qualified Data.ByteString.Lazy as BL
import Data.Text
import qualified Data.Text.Encoding as BL
import Database.PostgreSQL.Simple
import qualified Servant as S

newtype Env = Env
  { sqlConn :: Connection
  }

type ReaderIO a = ReaderT Env IO a

data ApiError = ApiError S.ServerError Text

throwApi :: ApiError -> ReaderIO a
throwApi = C.throwM . toServantError
  where
    toServantError (ApiError errCode err) = errCode {S.errBody = BL.fromStrict $ BL.encodeUtf8 err}

getSqlConnection :: ReaderIO Connection
getSqlConnection = do
  Env {..} <- ask
  return sqlConn
