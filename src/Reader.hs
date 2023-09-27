{-# LANGUAGE DataKinds #-}

module Reader where

import qualified Control.Monad.Catch as C
import Control.Monad.Trans.Reader
import qualified Data.ByteString.Lazy as BL
import Data.Text
import qualified Data.Text.Encoding as BL
import qualified Network.Google as Google
import qualified Servant as S

data Env = Env
  { sessionName :: Text,
    googleEnv :: Google.Env '["https://www.googleapis.com/auth/spanner.data"]
  }

type ReaderIO a = ReaderT Env IO a

data ApiError = ApiError S.ServerError Text

throwApi :: ApiError -> ReaderIO a
throwApi = C.throwM . toServantError
  where
    toServantError (ApiError errCode err) = errCode {S.errBody = BL.fromStrict $ BL.encodeUtf8 err}

getSessionName :: ReaderIO Text
getSessionName = do
  Env {..} <- ask
  return sessionName

getGoogleEnv :: ReaderIO (Google.Env '["https://www.googleapis.com/auth/spanner.data"])
getGoogleEnv = do
  Env {..} <- ask
  return googleEnv
