module Store.DDB where

import qualified Aws
import Aws.DynamoDb.Core (fromItem, toItem, hk,
  UpdateReturn(URAllOld), ReturnConsumption(RCTotal),
  ReturnItemCollectionMetrics(RICMSize), toValue, Attribute(Attribute))
import Aws.DynamoDb.Commands (getItem, qrItems, scan, query, Slice(Slice), qrItems, qIndex, putItem,
  piReturn, piRetCons, piRetMet, srItems)


put
  :: Item i
  => i
  -> Text
  -> IO ()
put item tableName = do
  let req = (putItem logsTable $ toItem entry ) {
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
