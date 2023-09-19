{-# LANGUAGE TemplateHaskell #-}

module Routes.Types where

import Data.Text
import Data.Aeson
import Data.Aeson.TH

data Book =
  Book
    { id :: Maybe Text
    , name :: Text
    , author :: Text
    }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''Book)
