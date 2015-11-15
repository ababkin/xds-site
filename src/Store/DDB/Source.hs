{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Store.DDB.Source where

import Control.Applicative        ((<$>))
import Data.Either (rights)
import qualified Data.Vector as V
import Data.Either.Unwrap (fromRight)
import Control.Monad (forM_)
import qualified Aws
import Aws.DynamoDb.Commands 
import Aws.DynamoDb.Core (fromItem, toItem, 
  UpdateReturn(URAllOld), ReturnConsumption(RCTotal), 
  ReturnItemCollectionMetrics(RICMSize))

import Types (Source(..))
import Store.DDB.Types ()

sourcesTable = "sources"

getSources :: IO [Source]
getSources =
  (rights . map fromItem . V.toList . srItems) <$> fromRight <$> runWithCreds (scan sourcesTable)


putSource 
  :: Source 
  -> IO ()
putSource source = do
  let req = (putItem sourcesTable $ toItem source ) { 
      piReturn  = URAllOld
    , piRetCons = RCTotal
    , piRetMet  = RICMSize
    }

  eitherResp <- runWithCreds req
  return ()

{- runWithCreds :: Aws.AWSRequest req => req -> IO (Either String resp) -}
runWithCreds r = do
  cfg <- Aws.baseConfiguration
  maybeCreds <- Aws.loadCredentialsFromEnv
  maybe
    (return $ Left "Please set the environment variables AWS_ACCESS_KEY_ID and AWS_ACCESS_KEY_SECRET")
    (\creds -> Right <$> Aws.simpleAws cfg{Aws.credentials = creds} Aws.debugServiceConfig r) 
    maybeCreds

