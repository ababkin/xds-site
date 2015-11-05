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
  , uid         :: Int
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
                <*> field

instance ToRow Source where
  toRow Source{uuid, uid, title, description,
                  url, createdAt, updatedAt} = [
                      toField uuid
                    , toField uid
                    , toField title
                    , toField description
                    , toField url
                    , toField createdAt
                    , toField updatedAt
                    ]


data UserRegistration = UserRegistration {
    firstName             :: Text
  , lastName              :: Text
  , username              :: Text
  , email                 :: Text
  , password              :: Text
  } deriving Show


data Login = Login
  { loginUsername :: Text  -- ^ username for login
  , loginPassword :: Text  -- ^ password for login
  , loginRemember :: Bool  -- ^ remember token for login
  } deriving (Show)


