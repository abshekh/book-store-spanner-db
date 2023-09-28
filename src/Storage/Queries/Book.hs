module Storage.Queries.Book
  ( selectOneById,
    selectAll,
    insertOneMaybe,
    updateOneMaybe,
    deleteOneMaybe,
  )
where

import Data.Aeson (decode, encode)
import Data.Text hiding (map)
import qualified Reader as R
import Routes.Types
import qualified Servant as S
import Storage.Queries.DBQueries

selectOneById :: Text -> R.ReaderIO (Maybe Book)
selectOneById id' = do
  let query = "select * from books where id = \"" <> id' <> "\" limit 1"
  maybeResult <- decode . encode <$> runDQL query
  case (maybeResult :: Maybe [[Text]]) of
    Just [[_id, _name, _author]] -> return . Just $ Book (Just _id) _name _author
    _ -> R.throwApi $ R.ApiError S.err500 "Could not parse books"

insertOneMaybe :: Book -> R.ReaderIO (Maybe Book)
insertOneMaybe Book {id = Just _id, author = _author, name = _name} = do
  let query = "insert into books (id, name, author) values (\"" <> _id <> "\", \"" <> _name <> "\", \"" <> _author <> "\") then return id, name, author"
  maybeResult <- decode . encode <$> runDML query
  case (maybeResult :: Maybe [[Text]]) of
    Just [[_id, _name, _author]] -> return . Just $ Book (Just _id) _name _author
    _ -> R.throwApi $ R.ApiError S.err500 "Could not parse books"
insertOneMaybe _ = return Nothing

updateOneMaybe :: Book -> R.ReaderIO (Maybe Book)
updateOneMaybe Book {id = Just _id, author = _author, name = _name} = do
  let query = "update books set name = \"" <> _name <> "\", author = \"" <> _author <> "\" where id = \"" <> _id <> "\"then return id, name, author"
  maybeResult <- decode . encode <$> runDML query
  case (maybeResult :: Maybe [[Text]]) of
    Just [[_id, _name, _author]] -> return . Just $ Book (Just _id) _name _author
    _ -> R.throwApi $ R.ApiError S.err500 "Could not parse books"
updateOneMaybe _ = return Nothing

selectAll :: R.ReaderIO [Book]
selectAll = do
  let query = "select * from books"
  maybeResult <- decode . encode <$> runDQL query
  case (maybeResult :: Maybe [[Text]]) of
    Just result -> return $ map (\[_id, _name, _author] -> Book (Just _id) _name _author) result
    Nothing -> R.throwApi $ R.ApiError S.err500 "Could not parse books"

deleteOneMaybe :: Text -> R.ReaderIO (Maybe Book)
deleteOneMaybe id' = do
  let query = "delete from books where id =\"" <> id' <> "\" then return id, name, author"
  maybeResult <- decode . encode <$> runDML query
  case (maybeResult :: Maybe [[Text]]) of
    Just [[_id, _name, _author]] -> return . Just $ Book (Just _id) _name _author
    _ -> R.throwApi $ R.ApiError S.err500 "Could not parse books"
