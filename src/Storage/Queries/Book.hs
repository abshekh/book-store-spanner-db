module Storage.Queries.Book
  ( selectOneById,
    selectAll,
    upsertOne,
    deleteOneMaybe,
  )
where

import qualified Reader as R
import Control.Applicative ((<|>))
import Data.Maybe (fromJust)
import Data.String (IsString (fromString))
import Data.Text
import Database.Beam ((==.))
import qualified Database.Beam as B
import qualified Storage.Queries.DBQueries as Q
import Storage.Types.Book
import qualified Storage.Types.DB as DB

dbTable :: B.DatabaseEntity be DB.BookStore (B.TableEntity BookT)
dbTable = DB.book . DB.bookStore (fromString "books") $ "public"

selectOneById :: Text -> R.ReaderH (Maybe (BookT B.Identity))
selectOneById id' = do
  Q.selectOneMaybe
    dbTable
    (\Book {..} -> B.sqlBool_ (bookId ==. B.val_ id'))

insertOneMaybe :: Book -> R.ReaderH (Maybe Book)
insertOneMaybe book = do
  Q.insertOne dbTable (insertExp book)

updateOneMaybe :: Book -> R.ReaderH (Maybe Book)
updateOneMaybe book = do
  Q.updateOne dbTable book

selectAll :: R.ReaderH [Book]
selectAll = do
  Q.selectAll dbTable

deleteOneMaybe :: Text -> R.ReaderH (Maybe Book)
deleteOneMaybe id' = do
  books <- Q.delete dbTable (\Book {..} -> bookId ==. B.val_ id')
  case books of
    [book] -> return $ Just book
    _ -> return Nothing

upsertOne :: Book -> R.ReaderH Book
upsertOne book = do
  insertBook <- insertOneMaybe book
  updateBook <- updateOneMaybe book
  return . fromJust $ updateBook <|> insertBook
