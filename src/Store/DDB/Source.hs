{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Store.DDB.Source where

import Control.Applicative        ((<$>))
import Data.Either (rights)
import qualified Data.Vector as V
import Data.Either.Unwrap (fromRight)
import Control.Monad (forM_)
import qualified Aws
import Aws.DynamoDb.Commands (getItem, qrItems, scan, query, Slice(Slice), qrItems, qIndex, putItem,
  piReturn, piRetCons, piRetMet, srItems)
import Aws.DynamoDb.Core (fromItem, toItem, hk,
  UpdateReturn(URAllOld), ReturnConsumption(RCTotal), 
  ReturnItemCollectionMetrics(RICMSize), toValue, Attribute(Attribute))
import Data.UUID.V4 (nextRandom)
import Data.Time.Clock (getCurrentTime)
import Data.Text (Text)
import Aws.DynamoDb (GetItemResponse(girItem))
import Data.Maybe (fromJust)

import Types (Source(..), NewSource(..), Dataset(..))
import Store.DDB.Types ()

sourcesTable  = "sources"
datasetsTable = "datasets"

getSources :: IO [Source]
getSources = do

    {- eitherResults <- (map fromItem . V.toList . srItems) <$> fromRight <$> runWithCreds (scan sourcesTable) -}
    {- mapM fetchSourceDatasets $ rights eitherResults -}

  resp <- runWithCreds (scan sourcesTable)
  either
    error
    (mapM fetchSourceDatasets . rights)
    (map fromItem . V.toList . srItems <$> resp)

  {- resp <- runWithCreds (scan sourcesTable) -}
  {- mapM fetchSourceDatasets $ rights (map fromItem . V.toList . srItems <$> resp)  -}

getSource :: Text -> IO (Either String Source)
getSource sourceId = do
  let req = getItem sourcesTable (hk "uuid" $ toValue sourceId)

  {- let q = query sourcesTable (Slice (Attribute "uuid" (toValue sourceId)) Nothing) -}

  resp <- runWithCreds req
  eitherDo (fromItem =<< maybeToEither "Could not get the item" . girItem =<< resp) fetchSourceDatasets


maybeToEither = (`maybe` Right) . Left 

eitherDo :: (Functor m, Monad m) => Either a b -> (b -> m c) -> m (Either a c)
eitherDo (Left x) _ = return $ Left x
eitherDo (Right y) f = Right <$> f y

fetchSourceDatasets source@Source{sUuid} = do
  let q = query datasetsTable (Slice (Attribute "sourceId" (toValue sUuid)) Nothing)
  datasets <- rights . map fromItem . V.toList . qrItems . fromRight <$>
    runWithCreds q{qIndex = Just "SourcesIndex"}
  return $ source{sDatasets = datasets}

createSource 
  :: NewSource 
  -> IO ()
createSource NewSource{nsUserId, nsTitle, nsDescription, nsUrl, nsDatasetUrl} = do
  newSourceUuid <- nextRandom
  timestamp     <- getCurrentTime

  putSource Source{
      sUuid        = newSourceUuid
    , sUserId      = nsUserId
    , sTitle       = nsTitle
    , sDescription = nsDescription
    , sUrl         = nsUrl
    , sCreatedAt   = timestamp
    , sUpdatedAt   = timestamp
    }

  case nsDatasetUrl of
    Just datasetUrl -> do
      newDatasetUuid  <- nextRandom

      putDataset Dataset{
          dsUuid        = newDatasetUuid
        , dsSourceId    = newSourceUuid
        , dsUserId      = nsUserId
        , dsTitle       = nsTitle
        , dsDescription = Nothing
        , dsUrl         = datasetUrl
        , dsCreatedAt   = timestamp
        , dsUpdatedAt   = timestamp
        }
    Nothing -> 
      return ()

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

putDataset
  :: Dataset
  -> IO ()
putDataset dataset = do
  let req = (putItem datasetsTable $ toItem dataset) { 
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

