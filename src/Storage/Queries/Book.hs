module Storage.Queries.Book
  ( selectOneById,
    selectAll,
    upsertOne,
    deleteOneMaybe,
  )
where

import Control.Applicative ((<|>))
import Data.Maybe (fromJust)
import Data.String (IsString (fromString))
import Data.Text
import Database.Beam ((==.))
import qualified Database.Beam as B
import qualified Database.PostgreSQL.Simple as PostgreSQL
import qualified Storage.Queries.DBQueries as Q
import Storage.Types.Book
import qualified Storage.Types.DB as DB

dbTable :: B.DatabaseEntity be DB.BookStore (B.TableEntity BookT)
dbTable = DB.book . DB.bookStore (fromString "books") $ "public"

selectOneById :: PostgreSQL.Connection -> Text -> IO (Maybe (BookT B.Identity))
selectOneById conn id' = do
  Q.selectOneMaybe
    conn
    dbTable
    (\Book {..} -> B.sqlBool_ (bookId ==. B.val_ id'))

insertOneMaybe :: PostgreSQL.Connection -> Book -> IO (Maybe Book)
insertOneMaybe conn book = do
  Q.insertOne conn dbTable (insertExp book)

updateOneMaybe :: PostgreSQL.Connection -> Book -> IO (Maybe Book)
updateOneMaybe conn book = do
  Q.updateOne conn dbTable book

selectAll :: PostgreSQL.Connection -> IO [Book]
selectAll conn = do
  Q.selectAll conn dbTable

deleteOneMaybe :: PostgreSQL.Connection -> Text -> IO (Maybe Book)
deleteOneMaybe conn id' = do
  books <- Q.delete conn dbTable (\Book {..} -> bookId ==. B.val_ id')
  case books of
    [book] -> return $ Just book
    _ -> return Nothing

upsertOne :: PostgreSQL.Connection -> Book -> IO Book
upsertOne conn book = do
  insertBook <- insertOneMaybe conn book
  updateBook <- updateOneMaybe conn book
  return . fromJust $ updateBook <|> insertBook
