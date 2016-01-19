{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE NamedFieldPuns      #-}

module Logger where

import Data.Text (Text)

import Logger.Types
import qualified Store.DDB as DDB

logsTable = "logs"

logD :: Text -> IO ()
logD = log . Generic Debug

logI :: Text -> IO ()
logI = log . Generic Info

logW :: Text -> IO ()
logW = log . Generic Warning

logE :: Text -> IO ()
logE = log . Generic Error

logEvent
  :: Action
  -> ResourceType
  -> IO ()
logEvent action resource = log $ Event action resource



log :: Entry -> IO ()
log entry = DDB.put entry logsTable
