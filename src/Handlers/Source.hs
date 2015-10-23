{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handlers.Source where

import Control.Applicative ((<$>), (<*>), (<|>), pure)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Maybe (isJust)
import Data.Monoid (Monoid, mempty)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime, UTCTime)
import Data.UUID (UUID)
import Snap.Core (method, redirect, Method(GET, POST))
import Snap.Snaplet
{- import           Snap.Snaplet.Auth -}
import Snap.Snaplet.Heist (heistLocal, render, withSplices)
import Heist ((##))
import Heist.Interpreted (mapSplices, runChildrenWithText)
import Snap.Snaplet.PostgresqlSimple (query_, execute, query)
import Data.UUID.V4 (nextRandom)

import Text.Digestive.Types(Result(..))
import Text.Digestive (Form)
import Text.Digestive.Form ((.:), text, check, validateM)
import Text.Digestive.Snap (runForm)
import Text.Digestive.Heist (digestiveSplices, bindDigestiveSplices)

import Network.HTTP.Client (HttpException, parseUrl)
import Network.HTTP (simpleHTTP, getRequest, getResponseCode)
import Control.Exception (catch)
import Control.Monad.Catch (MonadThrow, MonadCatch, Exception)

import Application (App)
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
      validateM urlIsConnected . 
      check "URL must have correct format" isFormattedLikeURL .
      check "URL must not be empty" isNotEmpty

    urlIsConnected 
      :: (Monad m, MonadIO m) 
      => Text 
      -> m (Result Text Text)
    urlIsConnected url = do
      code <- liftIO $ catch (getResponseCode =<< simpleHTTP (getRequest $ T.unpack url)) giveUp
      return $ case code of
        (2,_,_) -> Success url
        (3,_,_) -> Success url
        _       -> Error "Could not successfully ping the URL"
      where
        giveUp :: (MonadThrow m) => HttpException -> m (Int, Int, Int)
        giveUp = const $ return (5,0,0)


isFormattedLikeURL :: Text -> Bool
isFormattedLikeURL = isJust . parseUrl . T.unpack

isNotEmpty :: Text -> Bool
isNotEmpty = not . T.null



handleSources :: Handler App App ()
handleSources = method GET handleListSources <|> method POST handleSourceForm

handleNewSource :: Handler App App ()
handleNewSource = method GET handleSourceForm

handleSourceForm = do 
  uuid            <- liftIO nextRandom
  timestamp       <- liftIO getCurrentTime
  (view, result)  <- runForm "source" $ sourceForm uuid timestamp
  case result of
    Just newSource  -> do
      _ <- execute "INSERT INTO sources VALUES (?, ?, ?, ?, ?, ?)" newSource
      redirect "/"
    Nothing -> 
      heistLocal (bindDigestiveSplices view) $ render "sources/new"



handleListSources :: Handler App App ()
handleListSources = do
  sources <- query_ "SELECT * FROM sources"
  withSplices (sourcesSplices sources) $ render "sources/index"
  
  where
    sourcesSplices sources =
      "sources" ## mapSplices sourceSplice sources

      where
        sourceSplice Source{title, description, url} = 
          runChildrenWithText $ do
            "title"       ## title
            "description" ## description
            "url"         ## url

