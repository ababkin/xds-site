module Store.PG.Types where

import Types (Source(..), User(..))


instance FromRow User where
  fromRow = User 
                <$> field 
                <*> field
                <*> field

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
  toRow Source{sUuid, sUserId, sTitle, sDescription,
                  sUrl, sCreatedAt, sUpdatedAt} = [
                      toField sUuid
                    , toField sUserId
                    , toField sTitle
                    , toField sDescription
                    , toField sUrl
                    , toField sCreatedAt
                    , toField sUpdatedAt
                    ]


