module Storage.Queries.DBQueries where

import Control.Monad.Trans.Class (MonadTrans (lift))
import qualified Database.Beam as B
import qualified Database.Beam.Backend as B
import qualified Database.Beam.Backend.SQL.BeamExtensions as B
import Database.Beam.Postgres (Postgres, runBeamPostgresDebug)
import qualified Reader as R

selectOneMaybe ::
  ( B.Beamable table,
    B.Database be db,
    B.FromBackendRow be (table B.Identity),
    be ~ Postgres
  ) =>
  B.DatabaseEntity be db (B.TableEntity table) ->
  (table (B.QExpr be B.QBaseScope) -> B.QExpr be B.QBaseScope B.SqlBool) ->
  R.ReaderIO (Maybe (table B.Identity))
selectOneMaybe dbTable predicate = do
  conn <- R.getSqlConnection
  lift $
    runBeamPostgresDebug putStrLn conn $
      B.runSelectReturningOne $
        B.select $
          B.filter_' predicate $
            B.all_ dbTable

selectAll ::
  ( B.Beamable table,
    B.Database be db,
    B.FromBackendRow be (table B.Identity),
    be ~ Postgres
  ) =>
  B.DatabaseEntity be db (B.TableEntity table) ->
  R.ReaderIO [table B.Identity]
selectAll dbTable = do
  conn <- R.getSqlConnection
  lift $
    runBeamPostgresDebug putStrLn conn $
      B.runSelectReturningList $
        B.select $
          B.all_ dbTable

insertOne ::
  ( B.Beamable table,
    B.Generic (table B.Identity),
    B.Generic (table B.Exposed),
    B.FromBackendRow be (table B.Identity),
    be ~ Postgres
  ) =>
  B.DatabaseEntity Postgres db (B.TableEntity table) ->
  B.SqlInsertValues Postgres (table (B.QExpr Postgres s)) ->
  R.ReaderIO (Maybe (table B.Identity))
insertOne dbTable insertExp = do
  conn <- R.getSqlConnection
  list <-
    lift $
      runBeamPostgresDebug putStrLn conn $
        B.runInsertReturningList $
          B.insert dbTable insertExp
  case list of
    [l] -> return $ Just l
    _ -> return Nothing

updateOne ::
  ( B.Table table,
    B.Generic (table B.Identity),
    B.Generic (table B.Exposed),
    B.FromBackendRow be (table B.Identity),
    B.SqlValableTable be (B.PrimaryKey table),
    B.SqlValableTable be table,
    B.HasTableEquality be (B.PrimaryKey table),
    be ~ Postgres
  ) =>
  B.DatabaseEntity Postgres db (B.TableEntity table) ->
  table B.Identity ->
  R.ReaderIO (Maybe (table B.Identity))
updateOne dbTable val = do
  conn <- R.getSqlConnection
  list <-
    lift $
      runBeamPostgresDebug putStrLn conn $
        B.runUpdateReturningList $
          B.save dbTable val
  case list of
    [l] -> return $ Just l
    _ -> return Nothing

delete ::
  ( B.Beamable table,
    B.FromBackendRow be (table B.Identity),
    be ~ Postgres
  ) =>
  B.DatabaseEntity Postgres db (B.TableEntity table) ->
  (forall s. table (B.QExpr be s) -> B.QExpr be s Bool) ->
  R.ReaderIO [table B.Identity]
delete dbTable predicate = do
  conn <- R.getSqlConnection
  lift $ runBeamPostgresDebug putStrLn conn $
    B.runDeleteReturningList $
      B.delete dbTable predicate
