module Routes.Handler where

import qualified Servant as S
import qualified Reader as R
import Routes.Types
import qualified Storage.Types.Book as DB
import Storage.Queries.Book
import Servant (NoContent)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.Text as DT

getBook :: String -> R.ReaderH Book
getBook id' = do
  bookMaybe <- selectOneById (DT.pack id')
  case bookMaybe of
    Just book -> R.throwApi $ R.ApiError S.err404 "Book not found"
    Nothing -> R.throwApi $ R.ApiError S.err404 "Book not found"

getAllBooks :: R.ReaderH [Book]
getAllBooks = undefined
-- liftIO $ BR.getAllBooks getDBFile

postBook :: Book -> R.ReaderH Book
postBook book = undefined
-- liftIO $ BR.upsertBook getDBFile book

deleteBook :: String -> R.ReaderH NoContent
deleteBook id' = undefined
-- liftIO $ BR.deleteBook getDBFile id' >> return NoContent

