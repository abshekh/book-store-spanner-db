{-# LANGUAGE TemplateHaskell #-}

module Routes.Types where

import Data.Aeson
import Data.Aeson.TH
import Data.Text

data Book = Book
  { id :: Maybe Text,
    name :: Text,
    author :: Text
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''Book)
