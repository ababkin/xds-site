{-# LANGUAGE OverloadedStrings #-}

module Store.S3.Source where

import Data.Monoid ((<>))
import Network.HTTP.Conduit (httpLbs, parseUrl, newManager, tlsManagerSettings, HttpException(StatusCodeException), ManagerSettings, mkManagerSettings)
import qualified Data.Text as T
import Data.UUID (UUID, toText)
import Network.Connection (TLSSettings(TLSSettingsSimple))

import Xds.Aws.Types (URL, Path)
import Xds.Aws.Config (config)
import qualified Xds.Aws.S3 as S3 (upload)

bucketName = "xdataset"

upload :: Path -> UUID -> IO URL
upload path uuid = do
  let s3Bucket  = bucketName
      s3Path    = "datasets/" <> (toText uuid) <> ".csv"

  mgr     <- newManager managerSettings
  awsCfg  <- config mgr
  S3.upload mgr awsCfg path s3Bucket s3Path

  where
    managerSettings :: ManagerSettings
    managerSettings = mkManagerSettings tlsSettings sockSettings

    tlsSettings = TLSSettingsSimple True False False

    sockSettings = Nothing
