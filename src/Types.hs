{-# LANGUAGE NamedFieldPuns #-}

module Types where

import Control.Applicative ((<$>), (<*>))
import Data.Text (Text)
import Database.PostgreSQL.Simple.FromRow (FromRow, fromRow, field)
import Database.PostgreSQL.Simple.ToRow (ToRow, toRow)
import Database.PostgreSQL.Simple.ToField (toField)
import Data.UUID (UUID)
import Data.Time.Clock (UTCTime)


data Source = Source {
    uuid        :: UUID
  , title       :: Text
  , description :: Text
  , url         :: Text
  , createdAt   :: UTCTime
  , updatedAt   :: UTCTime
  } deriving Show

instance FromRow Source where
  fromRow = Source 
                <$> field 
                <*> field
                <*> field
                <*> field
                <*> field
                <*> field

instance ToRow Source where
  toRow Source{uuid, title, description,
                  url, createdAt, updatedAt} = [
                      toField uuid
                    , toField title
                    , toField description
                    , toField url
                    , toField createdAt
                    , toField updatedAt
                    ]


data UserRegistration = UserRegistration {
    firstName             :: Text
  , lastName              :: Text
  , login                 :: Text
  , email                 :: Text
  , password              :: Text
  , passwordConfirmation  :: Text
  } deriving Show


