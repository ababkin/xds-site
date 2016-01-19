{-# LANGUAGE NamedFieldPuns #-}

module Types where

import Control.Applicative ((<$>), (<*>))
import Data.Text (Text)
import Database.PostgreSQL.Simple.FromRow (FromRow, fromRow, field)
import Database.PostgreSQL.Simple.ToRow (ToRow, toRow)
import Database.PostgreSQL.Simple.ToField (toField)
import Data.UUID (UUID)
import Data.Time.Clock (UTCTime)

import Xds.Aws.Types (URL)


data NewSource = NewSource {
    nsUserId      :: Text
  , nsTitle       :: Text
  , nsDescription :: Maybe Text
  , nsUrls        :: NewSourceUrl
  } deriving Show

data NewSourceUrl = NewSourceUrl {
    nsuUuid       :: UUID
  , nsuSourceUrl  :: Maybe URL
  , nsuDatasetUrl :: Maybe URL
  } deriving Show

data Source = Source {
    sUuid         :: UUID
  , sUserId       :: Text
  , sTitle        :: Text
  , sDescription  :: Maybe Text
  , sUrl          :: Maybe Text
  , sDatasets     :: [Dataset]
  , sCreatedAt    :: UTCTime
  , sUpdatedAt    :: UTCTime
  } deriving Show

data Dataset = Dataset {
    dsUuid        :: UUID
  , dsSourceId    :: UUID
  , dsUserId      :: Text
  , dsTitle       :: Text
  , dsDescription :: Maybe Text
  , dsUrl         :: Text
  , dsCreatedAt   :: UTCTime
  , dsUpdatedAt   :: UTCTime
  } deriving Show


data User = User {
    uId           :: Int
  , uUsername     :: Text
  , uEmail        :: Text
}

data UserRegistration = UserRegistration {
    urFirstName   :: Text
  , urLastName    :: Text
  , urUsername    :: Text
  , urEmail       :: Text
  , urPassword    :: Text
  } deriving Show


data Login = Login {
    loginUsername :: Text  -- ^ username for login
  , loginPassword :: Text  -- ^ password for login
  , loginRemember :: Bool  -- ^ remember token for login
  } deriving (Show)


