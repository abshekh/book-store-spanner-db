{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Storage.Types.Book where

import Data.String (IsString (fromString))
import Data.Text
import Database.Beam
import Database.Beam.Schema.Tables

data BookT f = Book
  { bookId :: C f Text,
    bookName :: C f Text,
    bookAuthor :: C f Text
  }
  deriving (Generic, Beamable)

type Book = BookT Identity

type BookId = PrimaryKey BookT Identity

deriving instance Show Book

deriving instance Show BookId

deriving instance Eq Book

instance Table BookT where
  data PrimaryKey BookT f = BookId (C f Text) deriving (Generic, Beamable)
  primaryKey = BookId . bookId

bookEMod :: Text -> Text -> EntityModification (DatabaseEntity be db) be (TableEntity BookT)
bookEMod tableName schema =
  setEntitySchema (Just schema)
    <> setEntityName tableName
    <> modifyTableFields
      tableModification
        { bookId = fromString "id"
        , bookName = fromString "name"
        , bookAuthor = fromString "author"
        }

insertExp c = insertExps [c]
insertExps cs = insertExpressions (toRowExpression <$> cs)
  where
    toRowExpression Book {..} =
      Book
        (val_ bookId)
        (val_ bookName)
        (val_ bookAuthor)
