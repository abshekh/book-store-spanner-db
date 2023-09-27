module Storage.Queries.Book
  ( selectOneById,
    selectAll,
    insertOneMaybe,
    updateOneMaybe,
    deleteOneMaybe,
  )
where

import Conduit (MonadTrans (lift), runResourceT)
import Control.Lens
import Data.Aeson (decode, encode)
import Data.Text hiding (map)
import qualified Network.Google as Google
import qualified Network.Google.Spanner as Google
import qualified Reader as R
import Routes.Types
import qualified Servant as S

selectOneById :: Text -> R.ReaderIO (Maybe Book)
selectOneById id' = do
  sessionName <- R.getSessionName
  googleEnv <- R.getGoogleEnv
  let e = Google.executeSQLRequest & Google.esqlrSQL ?~ "select * from books where id = \"" <> id' <> "\" limit 1"
  let p = Google.projectsInstancesDatabasesSessionsExecuteSQL e sessionName
  resultSet' <- runResourceT . Google.runGoogle googleEnv $ Google.send p
  let maybeResult = decode $ encode (resultSet' ^. Google.rsRows) :: Maybe [[Text]]
  case maybeResult of
    Just [[_id, _name, _author]] -> return . Just $ Book (Just _id) _name _author
    _ -> R.throwApi $ R.ApiError S.err500 "Could not parse books"

insertOneMaybe :: Book -> R.ReaderIO (Maybe Book)
insertOneMaybe Book {id = Just _id, author = _author, name = _name} = do
  sessionName <- R.getSessionName
  googleEnv <- R.getGoogleEnv
  let txnOptions = Google.transactionOptions & Google.toReadWrite ?~ Google.readWrite
  let txnSelector = Google.transactionSelector & Google.tsBegin ?~ txnOptions
  let e' = Google.executeSQLRequest & Google.esqlrSQL ?~ "insert into books (id, name, author) values (\"" <> _id <> "\", \"" <> _name <> "\", \"" <> _author <> "\") then return id, name, author"
  let e = e' & Google.esqlrTransaction ?~ txnSelector
  let p = Google.projectsInstancesDatabasesSessionsExecuteSQL e sessionName
  resultSet' <- runResourceT . Google.runGoogle googleEnv $ Google.send p
  lift . print $ resultSet'
  -- commit this transaction
  let maybeResult = decode $ encode (resultSet' ^. Google.rsRows) :: Maybe [[Text]]
  case maybeResult of
    Just [[_id, _name, _author]] -> return . Just $ Book (Just _id) _name _author
    _ -> R.throwApi $ R.ApiError S.err500 "Could not parse books"
insertOneMaybe _ = return Nothing

updateOneMaybe :: Book -> R.ReaderIO (Maybe Book)
updateOneMaybe Book {id = Just _id, author = _author, name = _name} = do
  sessionName <- R.getSessionName
  googleEnv <- R.getGoogleEnv
  let txnOptions = Google.transactionOptions & Google.toReadWrite ?~ Google.readWrite
  let txnSelector = Google.transactionSelector & Google.tsBegin ?~ txnOptions
  let e' = Google.executeSQLRequest & Google.esqlrSQL ?~ "update books set name = \"" <> _name <> "\", author = \"" <> _author <> "\" where id = \"" <> _id <> "\"then return id, name, author"
  let e = e' & Google.esqlrTransaction ?~ txnSelector
  let p = Google.projectsInstancesDatabasesSessionsExecuteSQL e sessionName
  resultSet' <- runResourceT . Google.runGoogle googleEnv $ Google.send p
  lift . print $ resultSet' ^. Google.rsRows
  -- commit this transaction
  let maybeResult = decode $ encode (resultSet' ^. Google.rsRows) :: Maybe [[Text]]
  case maybeResult of
    Just [[_id, _name, _author]] -> return . Just $ Book (Just _id) _name _author
    _ -> R.throwApi $ R.ApiError S.err500 "Could not parse books"
updateOneMaybe _ = return Nothing

selectAll :: R.ReaderIO [Book]
selectAll = do
  sessionName <- R.getSessionName
  googleEnv <- R.getGoogleEnv
  let e = Google.executeSQLRequest & Google.esqlrSQL ?~ "select * from books"
  let p = Google.projectsInstancesDatabasesSessionsExecuteSQL e sessionName
  resultSet' <- runResourceT . Google.runGoogle googleEnv $ Google.send p
  let maybeResult = decode $ encode (resultSet' ^. Google.rsRows) :: Maybe [[Text]]
  case maybeResult of
    Just result -> return $ map (\[_id, _name, _author] -> Book (Just _id) _name _author) result
    Nothing -> R.throwApi $ R.ApiError S.err500 "Could not parse books"

deleteOneMaybe :: Text -> R.ReaderIO (Maybe Book)
deleteOneMaybe id' = do
  sessionName <- R.getSessionName
  googleEnv <- R.getGoogleEnv
  let txnOptions = Google.transactionOptions & Google.toReadWrite ?~ Google.readWrite
  let txnSelector = Google.transactionSelector & Google.tsBegin ?~ txnOptions
  let e' = Google.executeSQLRequest & Google.esqlrSQL ?~ "delete from books where id =\"" <> id' <> "\" then return id, name, author"
  let e = e' & Google.esqlrTransaction ?~ txnSelector
  let p = Google.projectsInstancesDatabasesSessionsExecuteSQL e sessionName
  resultSet' <- runResourceT . Google.runGoogle googleEnv $ Google.send p
  lift . print $ resultSet' ^. Google.rsRows
  -- commit this transaction
  let maybeResult = decode $ encode (resultSet' ^. Google.rsRows) :: Maybe [[Text]]
  case maybeResult of
    Just [[_id, _name, _author]] -> return . Just $ Book (Just _id) _name _author
    _ -> R.throwApi $ R.ApiError S.err500 "Could not parse books"
