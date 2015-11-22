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
import Text.Digestive.Form ((.:), text, optionalText, check, checkM)
import Network.HTTP.Client (HttpException, parseUrl)
import Network.HTTP (simpleHTTP, getRequest, getResponseCode)

import Forms.Validators (isFormattedLikeURL, isNotEmpty)
import Types (Source(..), NewSource(..))


sourceForm 
  :: (Monad m, MonadIO m)
  => Text
  -> Form Text m NewSource
sourceForm uid =
  NewSource <$> pure uid
            <*> "title"           .: check "Title must not be empty" isNotEmpty (text Nothing)
            <*> "description"     .: optionalText Nothing
            <*> "url"             .: (fst <$> validateUrlPresence)
            <*> "url.dataset_url" .: datasetUrlValidation (optionalText Nothing)

  where
    validateUrlPresence =
      check "Either Source URL or Dataset URL must be present:" (\(u, du) -> isJust $ u <> du) urlF
      where
        urlF = (\u du -> (u, du)) 
          <$> "webpage_url" .: webpageUrlValidation (optionalText Nothing)
          <*> "dataset_url" .: optionalText Nothing


    isMaybeFormattedLikeURL :: Maybe Text -> Bool
    isMaybeFormattedLikeURL Nothing = True
    isMaybeFormattedLikeURL (Just t) = isJust . parseUrl . T.unpack $ t

    datasetUrlValidation =
      check "URL must have a correct format. e.g.: http://example.com/some/data.csv" isMaybeFormattedLikeURL


    webpageUrlValidation = 
      checkM "Could not successfully ping the URL" isPingable . 
      check "URL must have a correct format. e.g.: http://example.com/some/data" isMaybeFormattedLikeURL

    isPingable 
      :: (Monad m, MonadIO m) 
      => Maybe Text 
      -> m Bool
    isPingable Nothing = return True
    isPingable (Just url) = do
      code <- liftIO $ catch (getResponseCode =<< simpleHTTP (getRequest $ T.unpack url)) giveUp
      return $ case code of
        (2,_,_) -> True
        (3,_,_) -> True
        _       -> False 
      where
        giveUp :: (MonadThrow m) => HttpException -> m (Int, Int, Int)
        giveUp = const $ return (5,0,0)

