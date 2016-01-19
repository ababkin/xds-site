{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleInstances   #-}

module Store.DDB.Types where

import Control.Applicative ((<$>), (<*>), pure)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON),
  Value (..), object, (.:), (.=))
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text)
import qualified Data.Text as T
import Aws.DynamoDb.Core (Item, fromValue, toValue, DynVal(..), DynString(..),
  FromDynItem(..), ToDynItem(..), fromItem, item, attr, getAttr, getAttr',
  attrAs, text, DynData(..))
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust, catMaybes)
import Data.Time.Format (parseTime, formatTime)
import System.Locale (defaultTimeLocale)
import Data.Char (toLower)
import Data.UUID (UUID, fromText, toText)

import Types (Source(..), Dataset(..))


instance DynVal (Maybe Text) where
  type DynRep (Maybe Text) = DynString
  fromRep (DynString t) =
    if T.null t
      then Nothing
      else Just (Just t)
  toRep = DynString . fromMaybe ""

instance DynVal UUID where
  type DynRep UUID = DynString
  fromRep (DynString t) = fromText t
  toRep = DynString . toText

instance FromDynItem Source where
  parseItem i = Source
    <$> getAttr "uuid" i
    <*> getAttr "userId" i
    <*> getAttr "title" i
    <*> getAttr' "description" i
    <*> getAttr' "url" i
    <*> pure []
    <*> getAttr "createdAt" i
    <*> getAttr "updatedAt" i

instance ToDynItem Source where
  toItem Source{sUuid, sUserId, sTitle, sDescription,
                sUrl, sCreatedAt, sUpdatedAt} =
    item $ attrs [
          attr "uuid"         sUuid
        , attr "userId"       sUserId
        , attr "title"        sTitle
        , attr "createdAt"    sCreatedAt
        , attr "updatedAt"    sUpdatedAt
        ]
        [
          optionalAttr "description" sDescription
        , optionalAttr "url" sUrl
        ]


instance FromDynItem Dataset where
  parseItem i = Dataset
    <$> getAttr "uuid" i
    <*> getAttr "sourceId" i
    <*> getAttr "userId" i
    <*> getAttr "title" i
    <*> getAttr' "description" i
    <*> getAttr "url" i
    <*> getAttr "createdAt" i
    <*> getAttr "updatedAt" i

instance ToDynItem Dataset where
  toItem Dataset{dsUuid, dsSourceId, dsUserId, dsTitle, dsDescription,
                  dsUrl, dsCreatedAt, dsUpdatedAt} =
    item $ attrs [
        attr "uuid"         dsUuid
      , attr "sourceId"     dsSourceId
      , attr "userId"       dsUserId
      , attr "title"        dsTitle
      , attr "url"          dsUrl
      , attr "createdAt"    dsCreatedAt
      , attr "updatedAt"    dsUpdatedAt
      ] [optionalAttr "description" dsDescription]

attrs :: [a] -> [Maybe a] -> [a]
attrs base opts = base ++ catMaybes opts

{- optionalAttr :: Text -> Maybe a -> Maybe _  -}
optionalAttr _ Nothing          = Nothing
optionalAttr attrName attrValue = Just $ attr attrName attrValue




