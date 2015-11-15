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
    sUuid        :: UUID
  , sUserId      :: Text
  , sTitle       :: Text
  , sDescription :: Text
  , sUrl         :: Text
  , sCreatedAt   :: UTCTime
  , sUpdatedAt   :: UTCTime
  } deriving Show


data User = User {
    uId                    :: Int
  , uUsername              :: Text
  , uEmail                 :: Text
}

data UserRegistration = UserRegistration {
    urFirstName             :: Text
  , urLastName              :: Text
  , urUsername              :: Text
  , urEmail                 :: Text
  , urPassword              :: Text
  } deriving Show


data Login = Login
  { loginUsername :: Text  -- ^ username for login
  , loginPassword :: Text  -- ^ password for login
  , loginRemember :: Bool  -- ^ remember token for login
  } deriving (Show)


