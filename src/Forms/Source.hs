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
import Network.HTTP.Client (HttpException, parseUrl)
import Network.HTTP (simpleHTTP, getRequest, getResponseCode, rspCode, Response)
import Network.HTTP.Headers (findHeader, HeaderName(HdrLocation))
import qualified Network.Stream as NS
import System.Posix (fileSize, getFileStatus)
import Network.TCP (HStream)
import qualified Data.ByteString.Lazy as L


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
      result <- catchAny ( simpleHTTP (getRequest $ T.unpack url)  ) giveUp 
      case result of
        Left _err -> return pingError
        Right resp ->
          case rspCode resp of
            (2,_,_) -> return . D.Success $ Just url
            (3,_,_) ->
              case findHeader HdrLocation resp of
                Nothing -> return pingError
                Just location -> return . D.Error $ "URL redirects to: " <> T.pack location <> " - consider using this URL instead"
        _       -> return pingError 
      where
        catchAny :: IO a -> (SomeException -> IO a) -> IO a
        catchAny = Control.Exception.catch

        giveUp :: SomeException -> IO (Either NS.ConnError (Response String))
        giveUp = const . return . Left $ NS.ErrorMisc pingErrorMessage

        pingError = D.Error $ T.pack pingErrorMessage
        
        pingErrorMessage = "Could not successfully ping the URL"


