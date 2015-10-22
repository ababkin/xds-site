module Types where

import Control.Applicative ((<$>), (<*>))
import Data.Text (Text)
import Database.PostgreSQL.Simple.FromRow


data Source = Source {
    title       :: Text
  , description :: Text
  , url         :: Text
  } deriving Show

instance FromRow Source where
  fromRow = Source 
                <$> field 
                <*> field
                <*> field

