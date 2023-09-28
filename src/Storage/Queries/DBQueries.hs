module Storage.Queries.DBQueries (runDQL, runDML) where

import Conduit (MonadTrans (lift), runResourceT)
import Control.Lens
import Data.Aeson (Value)
import Data.Maybe (fromJust)
import Data.Text hiding (map)
import qualified Network.Google as Google
import qualified Network.Google.Spanner as Google
import qualified Reader as R

runDQL :: Text -> R.ReaderIO [[Value]]
runDQL query = do
  sessionName <- R.getSessionName
  googleEnv <- R.getGoogleEnv
  let e = Google.executeSQLRequest & Google.esqlrSQL ?~ query
  let p = Google.projectsInstancesDatabasesSessionsExecuteSQL e sessionName
  resultSet' <- runResourceT . Google.runGoogle googleEnv $ Google.send p
  return (resultSet' ^. Google.rsRows)

runDML :: Text -> R.ReaderIO [[Value]]
runDML query = do
  sessionName <- R.getSessionName
  googleEnv <- R.getGoogleEnv

  let txnOptions = Google.transactionOptions & Google.toReadWrite ?~ Google.readWrite
  -- begin transaction
  let txnRequest = Google.beginTransactionRequest & Google.btrOptions ?~ txnOptions
  let projectTxnRequest = Google.projectsInstancesDatabasesSessionsBeginTransaction txnRequest sessionName
  transaction <- runResourceT . Google.runGoogle googleEnv $ Google.send projectTxnRequest
  let transactionId = fromJust (transaction ^. Google.tId)

  let txnSelector = Google.transactionSelector & Google.tsId ?~ transactionId
  let e' = Google.executeSQLRequest & Google.esqlrSQL ?~ query
  let e = e' & Google.esqlrTransaction ?~ txnSelector
  let p = Google.projectsInstancesDatabasesSessionsExecuteSQL e sessionName
  resultSet' <- runResourceT . Google.runGoogle googleEnv $ Google.send p
  lift . print $ resultSet'

  -- commit this transaction
  let commitRequest = Google.commitRequest & Google.crTransactionId ?~ transactionId
  let projectCommitRequest = Google.projectsInstancesDatabasesSessionsCommit commitRequest sessionName
  commitResponse' <- runResourceT . Google.runGoogle googleEnv $ Google.send projectCommitRequest
  let _ = fromJust (commitResponse' ^. Google.crCommitTimestamp)

  return (resultSet' ^. Google.rsRows)
