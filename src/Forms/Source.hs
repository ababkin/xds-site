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

import Store.S3.Source (upload)
import Application (AppHandler)
import Forms.Validators (isFormattedLikeURL, isNotEmpty)
import Types (Source(..), NewSource(..), NewSourceUrl(..))


sourceForm
  :: UUID
  -> Text
  -> Form Text AppHandler NewSource
sourceForm uuid uid =
  NewSource <$> pure uid
            <*> "title"       .: check "Title must not be empty" isNotEmpty (text Nothing)
            <*> "description" .: optionalText Nothing
            <*> "url"         .: datasetValidation uuid

  where
    isMaybeFormattedLikeURL :: Maybe Text -> Bool
    isMaybeFormattedLikeURL Nothing = True
    isMaybeFormattedLikeURL (Just t) = isJust . parseUrl . T.unpack $ t


    datasetValidation uuid = validateM (extractUrls uuid) $
                     (,,) <$> "webpage" .: webpageUrlValidation (optionalText Nothing)
                          <*> "remote"  .: remoteDatasetUrlValidation (optionalText Nothing)
                          <*> "local"   .: file


      where

        extractUrls
          :: UUID
          -> (Maybe Text, Maybe Text, Maybe FilePath)
          -> AppHandler (D.Result Text NewSourceUrl)

        extractUrls _ (Nothing, Nothing, Nothing) =
          return $ D.Error "Please enter Webpage URL, Dataset URL or upload a Dataset file"

        extractUrls uuid (maybeWebsiteUrl, maybeRemoteDatasetUrl, Nothing) =
          return . D.Success $ NewSourceUrl uuid maybeWebsiteUrl maybeRemoteDatasetUrl

        extractUrls uuid (maybeWebsiteUrl, maybeRemoteDatasetUrl, Just path) = do
          size <- fileSize <$> liftIO (getFileStatus path)
          if size == 0
            then extractUrls uuid (maybeWebsiteUrl, maybeRemoteDatasetUrl, Nothing)
            else
              if isJust maybeRemoteDatasetUrl
                then return $ D.Error "Please either enter a remote URL OR attach a file, not both"
                else liftIO $ do
                  putStrLn $ "filepath received: " ++ path
                  url <- upload (T.pack path) uuid
                  return . D.Success $ NewSourceUrl uuid maybeWebsiteUrl $ Just url



        remoteDatasetUrlValidation =
          validateM (liftIO . pingValidation) .
          check "URL must have a correct format. e.g.: http://example.com/some/data.csv" isMaybeFormattedLikeURL


        webpageUrlValidation =
          validateM (liftIO . pingValidation) .
          check "URL must have a correct format. e.g.: http://example.com/some/data" isMaybeFormattedLikeURL

        pingValidation
          :: Maybe Text
          -> IO (D.Result Text (Maybe Text))
        pingValidation Nothing = return $ D.Success Nothing
        pingValidation (Just url) = do
          request <- parseUrl $ T.unpack url
          manager <- newManager tlsManagerSettings
          r <- fmap Right (httpLbs request manager)
            `catch` (\(StatusCodeException s _ _) -> do
              let code = statusCode s
              return . Left $ "Got error code: " ++ show code ++ case code of
                        403 -> ": Not Authorized"
                        404 -> ": Not Found"
                        _   -> ""
            )
            `catchAny` const (return $ Left "Unknown error")

          return $ case r of
            Right _   -> D.Success $ Just url
            Left err  -> D.Error . T.pack $ "Could not successfully ping the URL: " ++ err


          where
            catchAny :: IO a -> (SomeException -> IO a) -> IO a
            catchAny = Control.Exception.catch


