module Routes.Handler where

import Control.Monad.Cont (MonadTrans (lift))
import qualified Data.Text as DT
import qualified Data.UUID.V4 as UUID
import qualified Reader as R
import Routes.Types
import Servant (NoContent)
import qualified Servant as S
import Storage.Queries.Book
import Prelude hiding (id)

getBook :: String -> R.ReaderIO Book
getBook id' = do
  bookMaybe <- selectOneById (DT.pack id')
  case bookMaybe of
    Just book -> return book
    Nothing -> R.throwApi $ R.ApiError S.err404 "Book not found"

getAllBooks :: R.ReaderIO [Book]
getAllBooks = selectAll

postBook :: Book -> R.ReaderIO Book
postBook book@(Book {id = _id}) = do
  case _id of
    Just _ -> updateBook book
    Nothing -> insertBook book

updateBook :: Book -> R.ReaderIO Book
updateBook book = do
  bookMaybe <- updateOneMaybe book
  case bookMaybe of
    Just book' -> return book'
    Nothing -> R.throwApi $ R.ApiError S.err400 "Bad Request"

insertBook :: Book -> R.ReaderIO Book
insertBook book = do
  uuid <- lift UUID.nextRandom
  let book' = book {id = Just . DT.pack $ show uuid}
  bookMaybe <- insertOneMaybe book'
  case bookMaybe of
    Just book'' -> return book''
    Nothing -> R.throwApi $ R.ApiError S.err400 "Bad Request"

deleteBook :: String -> R.ReaderIO NoContent
deleteBook id' = do
  books <- deleteOneMaybe (DT.pack id')
  case books of
    Just _ -> return S.NoContent
    Nothing -> R.throwApi $ R.ApiError S.err404 "Book not found"
