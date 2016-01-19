{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE TypeFamilies      #-}

module Logger.Types where

import Control.Applicative ((<$>), (<*>))
import Data.Text (Text, pack, unpack)
import Aws.DynamoDb.Core (Item, fromValue, toValue, DynVal(..), DynString(..),
  FromDynItem(..), ToDynItem(..), fromItem, item, attr, getAttr, getAttr',
  attrAs, text, DynData(..))
import Data.Time.Clock (UTCTime)


toStringRep = DynString . pack . show

data Level = Debug | Info | Warning | Error deriving (Read, Show)
instance DynVal Level where
  type DynRep Level = DynString
  fromRep (DynString t) = read $ unpack t
  toRep = toStringRep


data ResourceType = Source | Dataset deriving (Read, Show)
instance DynVal ResourceType where
  type DynRep ResourceType = DynString
  fromRep (DynString t) = read $ unpack t
  toRep = toStringRep


data Entry = Entry {
    ePayload   :: EntryPayload
  , eTimestamp :: UTCTime
  }

data Action = Create | Update deriving (Read, Show)
instance DynVal Action where
  type DynRep Action = DynString
  fromRep (DynString t) = read $ unpack t
  toRep = toStringRep


data EntryPayload =
  Generic{
    gLevel    :: Level
  , gMessage  :: Text
  } |
  Event {
    eAction   :: Action
  , eResource :: ResourceType
  }


instance FromDynItem Entry where
  parseItem i = Entry
    <$> parsePayload i
    <*> getAttr "timestamp" i

    where
      parsePayload i = do
        typ <- getAttr "type" i
        return $ case typ of
          "generic" -> Generic
            <$> getAttr "level" i
            <*> getAttr "message" i
          "event" -> do
            action    <- getAttr "action" i
            resource  <- getAttr "resource" i
            return $ Event action resource

instance ToDynItem Entry where
  toItem Entry{ePayload, eTimestamp} =
    item $ attr "timestamp" "generic" : payloadItemAttrs ePayload

    where
      payloadItemAttrs Generic{gLevel, gMessage} =
        [
          attr "type"     "generic"
        , attr "level"    gLevel
        , attr "message"  gMessage
        ]
      payloadItemAttrs Event{eAction, eResource} =
        [
          attr "type"     "event"
        , attr "action"   eAction
        , attr "resource" eResource
        ]

