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
import Snap.Snaplet.PostgresqlSimple (query_, execute, query)
import Text.Digestive (Form)
import Text.Digestive.Form ((.:), text, check, checkM)
import Text.Digestive.Heist (digestiveSplices, bindDigestiveSplices)
import Text.Digestive.Snap (runForm)
import Text.Digestive.Types(Result(..))

import Application (App, auth)
import Types (Source(..))
import Forms.Source (sourceForm)



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
  loggedIn <- with auth isLoggedIn
  if loggedIn
    then do
      sources <- query_ "SELECT * FROM sources"
      withSplices (sourcesSplices sources) $ render "sources/index"

    else
      redirect "/login"
      
  where
    sourcesSplices sources =
      "sources" ## mapSplices sourceSplice sources

      where
        sourceSplice Source{title, description, url} = 
          runChildrenWithText $ do
            "title"       ## title
            "description" ## description
            "url"         ## url

