module Storage.Queries.DBQueries where

import qualified Database.Beam as B
import qualified Database.Beam.Backend as B
import qualified Database.Beam.Backend.SQL.BeamExtensions as B
import Database.Beam.Postgres (Postgres, runBeamPostgresDebug)
import qualified Database.PostgreSQL.Simple as PostgreSQL

selectOneMaybe ::
  ( B.Beamable table,
    B.Database be db,
    B.FromBackendRow be (table B.Identity),
    be ~ Postgres
  ) =>
  PostgreSQL.Connection ->
  B.DatabaseEntity be db (B.TableEntity table) ->
  (table (B.QExpr be B.QBaseScope) -> B.QExpr be B.QBaseScope B.SqlBool) ->
  IO (Maybe (table B.Identity))
selectOneMaybe conn dbTable predicate = do
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
  PostgreSQL.Connection ->
  B.DatabaseEntity be db (B.TableEntity table) ->
  IO [table B.Identity]
selectAll conn dbTable = do
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
  PostgreSQL.Connection ->
  B.DatabaseEntity Postgres db (B.TableEntity table) ->
  B.SqlInsertValues Postgres (table (B.QExpr Postgres s)) ->
  IO (Maybe (table B.Identity))
insertOne conn dbTable insertExp = do
  list <-
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
  PostgreSQL.Connection ->
  B.DatabaseEntity Postgres db (B.TableEntity table) ->
  table B.Identity ->
  IO (Maybe (table B.Identity))
updateOne conn dbTable val = do
  list <-
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
  PostgreSQL.Connection ->
  B.DatabaseEntity Postgres db (B.TableEntity table) ->
  (forall s. table (B.QExpr be s) -> B.QExpr be s Bool) ->
  IO [table B.Identity]
delete conn dbTable predicate = do
  runBeamPostgresDebug putStrLn conn $
    B.runDeleteReturningList $
      B.delete dbTable predicate
