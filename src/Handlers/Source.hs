{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handlers.Source where

import Control.Applicative ((<$>), (<*>), (<|>), pure)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid, mempty, (<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Heist ((##))
import Heist.Interpreted (mapSplices, runChildrenWithText)

import Snap.Core (method, redirect, Method(GET, POST))
import Snap.Snaplet (Handler, with)
import Snap.Snaplet.Auth (isLoggedIn, UserId(UserId), currentUser, AuthUser(userId))
import Snap.Snaplet.Heist (heistLocal, render, withSplices)
import Text.Digestive (Form)
import Text.Digestive.Form ((.:), text, check, checkM)
import Text.Digestive.Heist (digestiveSplices, bindDigestiveSplices)
import Text.Digestive.Snap (runForm)
import Text.Digestive.Types(Result(..))
import Snap.Extras.FlashNotice (flashSuccess, flashError)

import Application (App, AppHandler, auth, sess)
import Types (Source(..), NewSource(..))
import Forms.Source (sourceForm)
import Utils (ensureLoggedIn)
import Mixpanel (track)
import Store.DDB.Source (createSource, putSource, getSources)

sourcesHandler :: AppHandler ()
sourcesHandler = method GET listSourcesHandler <|> method POST sourceFormHandler

newSourceHandler :: AppHandler ()
newSourceHandler = method GET sourceFormHandler

sourceFormHandler = ensureLoggedIn $ do 
  maybeUser       <- with auth currentUser
  let (UserId uid) = fromMaybe (UserId "0") (userId =<< maybeUser) 
  (view, result)  <- runForm "source" $ sourceForm uid
  case result of
    Just newSource@NewSource{nsTitle} -> do
      liftIO $ createSource newSource
      flashSuccess sess $ "Successfully added " <> nsTitle <> " source"
      liftIO $ track "source-create"
      redirect "/"
    Nothing -> 
      heistLocal (bindDigestiveSplices view) $ render "sources/new"


listSourcesHandler :: AppHandler ()
listSourcesHandler = do
  sources <- liftIO getSources
  withSplices (sourcesSplices sources) $ render "sources/index"

  liftIO $ track "source-index"

  where
    sourcesSplices sources =
      "sources" ## mapSplices sourceSplice sources

      where
        sourceSplice Source{sTitle, sDescription, sUrl, sDatasets} = 
          runChildrenWithText $ do
            "title"       ## sTitle
            "description" ## fromMaybe "" sDescription
            "numDatasets" ## T.pack . show $ length sDatasets

