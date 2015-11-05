{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Mixpanel where

import Control.Lens ((^.))
import qualified Data.ByteString.Base64 as B64
import Network.Wreq
import Data.Text.Encoding (decodeUtf8)
import Data.Text (Text)
import qualified Data.Aeson as A
import qualified Data.Text as T
import Data.Monoid ((<>))
import qualified Data.ByteString.Lazy.Char8 as BL8
import GHC.Generics

data Properties = Properties {
    token :: Text
  } deriving (Show, Generic)
instance A.ToJSON Properties

data Event = Event {
    event :: Text
  , properties :: Properties
  } deriving (Show, Generic)
instance A.ToJSON Event




track :: Text -> IO ()
track e = do
  let event = Event e $ Properties "767b26ef69581c1e634bfc4a2e9e6fd1"
  r <- get $ toUrl event
  putStrLn $ case r ^. responseBody of
    "1" -> "event tracked successfully: " <> T.unpack e
    "0" -> "Error: failed to track event: " <> show r
    x   -> "Error: unknown track result received: " <> BL8.unpack x

  where
    toUrl :: Event -> String
    toUrl event = T.unpack $ "http://api.mixpanel.com/track/?data=" <> (encode64json event)
    
    encode64json = decodeUtf8 . B64.encode . BL8.toStrict . A.encode

