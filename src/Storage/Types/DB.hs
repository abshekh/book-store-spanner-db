module Storage.Types.DB where

import Data.Text
import Database.Beam
import qualified Storage.Types.Book as Book

newtype BookStore f = BookStore
  { book :: f (TableEntity Book.BookT)
  } deriving (Generic, Database be)

bookStore :: Text -> Text -> DatabaseSettings be BookStore
bookStore tableName schema =
  defaultDbSettings `withDbModification`
  dbModification
    { book = Book.bookEMod tableName schema
    }
