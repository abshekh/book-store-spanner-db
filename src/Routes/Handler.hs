module Routes.Handler where

import Prelude hiding (id)
import Control.Monad.Cont (MonadTrans (lift))
import qualified Data.Text as DT
import qualified Data.UUID.V4 as UUID
import qualified Reader as R
import Routes.Types
import Servant (NoContent)
import qualified Servant as S
import Storage.Queries.Book
import qualified Storage.Types.Book as DB

getBook :: String -> R.ReaderIO Book
getBook id' = do
  bookMaybe <- selectOneById (DT.pack id')
  case bookMaybe of
    Just book -> return $ dbBookToBook book
    Nothing -> R.throwApi $ R.ApiError S.err404 "Book not found"

getAllBooks :: R.ReaderIO [Book]
getAllBooks = do
  books <- selectAll
  return $ dbBookToBook <$> books

postBook :: Book -> R.ReaderIO Book
postBook book = do
  case id book of
    Just _ -> updateBook book
    Nothing -> insertBook book

updateBook :: Book -> R.ReaderIO Book
updateBook book = do
  bookMaybe <- updateOneMaybe (bookToDbBook book)
  case bookMaybe of
    Just book' -> return $ dbBookToBook book'
    Nothing -> R.throwApi $ R.ApiError S.err400 "Bad Request"

insertBook :: Book -> R.ReaderIO Book
insertBook book = do
  uuid <- lift UUID.nextRandom
  let book' = book {id = Just . DT.pack $ show uuid}
  bookMaybe <- insertOneMaybe (bookToDbBook book')
  case bookMaybe of
    Just book'' -> return $ dbBookToBook book''
    Nothing -> R.throwApi $ R.ApiError S.err400 "Bad Request"

deleteBook :: String -> R.ReaderIO NoContent
deleteBook id' = do
  books <- deleteOneMaybe (DT.pack id')
  case books of
    Just _ -> return S.NoContent
    Nothing -> R.throwApi $ R.ApiError S.err404 "Book not found"

dbBookToBook :: DB.Book -> Book
dbBookToBook DB.Book {..} =
  Book
    { id = Just bookId,
      name = bookName,
      author = bookAuthor
    }

bookToDbBook :: Book -> DB.Book
bookToDbBook Book {id = Just id', ..} =
  DB.Book
    { bookId = id',
      bookName = name,
      bookAuthor = author
    }
bookToDbBook _ = error "Something went wrong"
