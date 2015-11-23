{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Forms.Source where

import Control.Applicative ((<$>), (<*>), (<|>), pure)
import Control.Exception (catch, SomeException)
import Control.Monad.Catch (MonadThrow, MonadCatch, Exception)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import Data.Time.Clock (getCurrentTime, UTCTime)
import Text.Digestive (Form)
import Text.Digestive.Form ((.:), text, optionalText, check, checkM, validateM, file)
import qualified Text.Digestive.Types as D (Result(Success, Error))
import Network.HTTP.Conduit (httpLbs, parseUrl, newManager, tlsManagerSettings, HttpException(StatusCodeException))
import Network.HTTP.Types.Status (statusCode)
import System.Posix (fileSize, getFileStatus)

import Application (AppHandler)
import Forms.Validators (isFormattedLikeURL, isNotEmpty)
import Types (Source(..), NewSource(..))


sourceForm 
  :: Text
  -> Form Text AppHandler NewSource
sourceForm uid =
  NewSource <$> pure uid
            <*> "title"       .: check "Title must not be empty" isNotEmpty (text Nothing)
            <*> "description" .: optionalText Nothing
            <*> "url" .: datasetUrlValidation

  where
    isMaybeFormattedLikeURL :: Maybe Text -> Bool
    isMaybeFormattedLikeURL Nothing = True
    isMaybeFormattedLikeURL (Just t) = isJust . parseUrl . T.unpack $ t


    datasetUrlValidation = validateM extractUrls $
                     (,,) <$> "webpage" .: webpageUrlValidation (optionalText Nothing)
                          <*> "remote"  .: check "URL must have a correct format. e.g.: http://example.com/some/data.csv" isMaybeFormattedLikeURL (optionalText Nothing)
                          <*> "local"   .: file 


      where extractUrls :: (Maybe Text, Maybe Text, Maybe FilePath) -> AppHandler (D.Result Text (Maybe Text, Maybe Text))
            
            extractUrls (Nothing, Nothing, Nothing) = 
              return $ D.Error "Please enter Webpage URL, Dataset URL or upload a Dataset file"

            extractUrls (maybeWebsiteUrl, maybeRemoteDatasetUrl, Nothing) = 
              return $ D.Success (maybeWebsiteUrl, maybeRemoteDatasetUrl)
            
            extractUrls (maybeWebsiteUrl, maybeRemoteDatasetUrl, Just path) = do
              size <- fileSize <$> liftIO (getFileStatus path)
              if size == 0
                then extractUrls (maybeWebsiteUrl, maybeRemoteDatasetUrl, Nothing)
                else
                  if isJust maybeRemoteDatasetUrl 
                    then return $ D.Error "Please either enter a remote URL OR attach a file, not both"
                    else do
                      {- store <- use filestore -}
                      {- url <- storeFile store path Nothing -}
                      liftIO . putStrLn $ "filepath received: " ++ show path
                      return $ D.Success (maybeWebsiteUrl, Just "http://s3.com/somepath")



    webpageUrlValidation = 
      validateM (liftIO . isPingable) . 
      check "URL must have a correct format. e.g.: http://example.com/some/data" isMaybeFormattedLikeURL

    isPingable 
      :: Maybe Text 
      -> IO (D.Result Text (Maybe Text))
    isPingable Nothing = return $ D.Success Nothing 
    isPingable (Just url) = do
      request <- parseUrl $ T.unpack url 
      manager <- newManager tlsManagerSettings
      r <- fmap Right (httpLbs request manager) 
        `catch` (\(StatusCodeException s _ _) -> do
          let code = statusCode s
          return . Left $ case code of
                    403 -> "Not Authorized" 
                    404 -> "Not Found" 
                    _   -> "Got error code: " ++ show code 
        )
        `catchAny` const (return $ Left "Unknown error")

      return $ case r of
        Right _   -> D.Success $ Just url
        Left err  -> D.Error . T.pack $ "Could not successfully ping the URL: " ++ err


      where
        catchAny :: IO a -> (SomeException -> IO a) -> IO a
        catchAny = Control.Exception.catch


