{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handlers.Source where

import Control.Applicative ((<$>), (<*>), (<|>), pure)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Monoid (Monoid, mempty)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime, UTCTime)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Heist ((##))
import Heist.Interpreted (mapSplices, runChildrenWithText)

import Snap.Core (method, redirect, Method(GET, POST))
import Snap.Snaplet (Handler, with)
import Snap.Snaplet.Auth (isLoggedIn)
import Snap.Snaplet.Heist (heistLocal, render, withSplices)
import Text.Digestive (Form)
import Text.Digestive.Form ((.:), text, check, checkM)
import Text.Digestive.Heist (digestiveSplices, bindDigestiveSplices)
import Text.Digestive.Snap (runForm)
import Text.Digestive.Types(Result(..))

import Application (App, AppHandler, auth)
import Types (Source(..))
import Forms.Source (sourceForm)
import Utils (ensureLoggedIn)
import Store.Source (fetchAll, store)

sourcesHandler :: AppHandler ()
sourcesHandler = method GET listSourcesHandler <|> method POST sourceFormHandler

newSourceHandler :: AppHandler ()
newSourceHandler = method GET sourceFormHandler

sourceFormHandler = ensureLoggedIn $ do 
  uuid            <- liftIO nextRandom
  timestamp       <- liftIO getCurrentTime
  (view, result)  <- runForm "source" $ sourceForm uuid timestamp
  case result of
    Just newSource  -> do
      store newSource
      redirect "/"
    Nothing -> 
      heistLocal (bindDigestiveSplices view) $ render "sources/new"



listSourcesHandler :: AppHandler ()
listSourcesHandler = do
  sources <- fetchAll
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

