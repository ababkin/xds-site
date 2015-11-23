{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Forms.Source where

import Control.Applicative ((<$>), (<*>), (<|>), pure)
import Control.Exception (catch)
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
import Text.Digestive.Types (Result(Success, Error))
import Network.HTTP.Client (HttpException, parseUrl)
import Network.HTTP (simpleHTTP, getRequest, getResponseCode)
import Network.HTTP.Headers (findHeader, HeaderName(HdrLocation))
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


      where extractUrls :: (Maybe Text, Maybe Text, Maybe FilePath) -> AppHandler (Result Text (Maybe Text, Maybe Text))
            
            extractUrls (Nothing, Nothing, Nothing) = 
              return $ Error "Please enter Webpage URL, Dataset URL or upload a Dataset file"

            extractUrls (maybeWebsiteUrl, maybeRemoteDatasetUrl, Nothing) = 
              return $ Success (maybeWebsiteUrl, maybeRemoteDatasetUrl)
            
            extractUrls (maybeWebsiteUrl, maybeRemoteDatasetUrl, Just path) = do
              size <- fileSize <$> liftIO (getFileStatus path)
              if size == 0
                then extractUrls (maybeWebsiteUrl, maybeRemoteDatasetUrl, Nothing)
                else
                  if isJust maybeRemoteDatasetUrl 
                    then return $ Error "Please either enter a remote URL OR attach a file, not both"
                    else do
                      {- store <- use filestore -}
                      {- url <- storeFile store path Nothing -}
                      liftIO . putStrLn $ "filepath received: " ++ show path
                      return $ Success (maybeWebsiteUrl, Just "http://s3.com/somepath")



    webpageUrlValidation = 
      validateM (liftIO . isPingable) . 
      check "URL must have a correct format. e.g.: http://example.com/some/data" isMaybeFormattedLikeURL

    isPingable 
      :: Maybe Text 
      -> IO (Result Text (Maybe Text))
    isPingable Nothing = return $ Success Nothing 
    isPingable (Just url) = do
      {- resp <- liftIO $ catch (simpleHTTP (getRequest $ T.unpack url) ) giveUp -}
      result <- simpleHTTP (getRequest $ T.unpack url)
      code <- liftIO $ catch (getResponseCode resp) giveUp
      print code
      case code of
        (2,_,_) -> return . Success $ Just url
        (3,_,_) ->
          case result of
            Left _err -> return pingError
            Right resp ->
              case findHeader HdrLocation resp of
                Nothing -> return pingError
                Just location -> isPingable . Just $ T.pack location
        _       -> return pingError 
      where
        giveUp :: (MonadThrow m) => HttpException -> m (Int, Int, Int)
        giveUp = const $ return (5,0,0)

        pingError = Error "Could not successfully ping the URL"

