{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE TypeFamilies        #-}

module Store.DDB.Types where

import           Control.Applicative ((<$>), (<*>))
import           Data.Aeson          (FromJSON (parseJSON), ToJSON (toJSON),
                                      Value (..), object, (.:), (.=))
import           Data.Aeson.Types    (typeMismatch)
import           Data.Text           (Text)
import           Aws.DynamoDb.Core (Item, fromValue, toValue, DynVal(..), DynString(..),
  FromDynItem(..), ToDynItem(..), fromItem, item, attr, getAttr, attrAs, text, DynData(..))
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Time.Format (parseTime, formatTime)
import System.Locale (defaultTimeLocale)
import Data.Char (toLower)
import Data.UUID (UUID, fromText, toText)

import Types (Source(..))


instance DynVal UUID where
  type DynRep UUID = DynString
  fromRep (DynString t) = fromText t
  toRep = DynString . toText

instance FromDynItem Source where
  parseItem i = Source
    <$> getAttr "uuid" i
    <*> getAttr "userId" i
    <*> getAttr "title" i
    <*> getAttr "description" i
    <*> getAttr "url" i
    <*> getAttr "createdAt" i
    <*> getAttr "updatedAt" i

instance ToDynItem Source where
  toItem Source{sUuid, sUserId, sTitle, sDescription, sUrl, sCreatedAt, sUpdatedAt} = 
    item [
      attr "uuid"         sUuid
    , attr "userId"       sUserId 
    , attr "title"        sTitle 
    , attr "description"  sDescription 
    , attr "url"          sUrl
    , attr "createdAt"    sCreatedAt
    , attr "updatedAt"    sUpdatedAt
    ]

