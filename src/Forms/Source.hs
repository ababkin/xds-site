{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Forms.Source where

import Control.Applicative ((<$>), (<*>), (<|>), pure)
import Control.Exception (catch)
import Control.Monad.Catch (MonadThrow, MonadCatch, Exception)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import Data.Time.Clock (getCurrentTime, UTCTime)
import Text.Digestive (Form)
import Text.Digestive.Form ((.:), text, check, checkM)
import Network.HTTP.Client (HttpException, parseUrl)
import Network.HTTP (simpleHTTP, getRequest, getResponseCode)

import Forms.Validators (isFormattedLikeURL, isNotEmpty)
import Types (Source(..))


sourceForm 
  :: (Monad m, MonadIO m) 
  => UUID 
  -> UTCTime 
  -> Form Text m Source
sourceForm uuid timestamp = do
  Source  <$> pure uuid  
          <*> "title"       .: check "Title must not be empty" isNotEmpty (text Nothing)
          <*> "description" .: text Nothing
          <*> "url"         .: urlValidation (text Nothing)
          <*> pure timestamp
          <*> pure timestamp

  where
    urlValidation = 
      checkM "Could not successfully ping the URL" isPingable . 
      check "URL must have correct format" isFormattedLikeURL .
      check "URL must not be empty" isNotEmpty

    isPingable 
      :: (Monad m, MonadIO m) 
      => Text 
      -> m Bool
    isPingable url = do
      code <- liftIO $ catch (getResponseCode =<< simpleHTTP (getRequest $ T.unpack url)) giveUp
      return $ case code of
        (2,_,_) -> True
        (3,_,_) -> True
        _       -> False 
      where
        giveUp :: (MonadThrow m) => HttpException -> m (Int, Int, Int)
        giveUp = const $ return (5,0,0)

