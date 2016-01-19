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
import Data.UUID (UUID, toText)
import Heist ((##))
import Heist.Interpreted (mapSplices, runChildrenWithText, textSplice)
import Data.UUID.V4 (nextRandom)

import Snap.Core (method, redirect, Method(GET, POST), getParam, MonadSnap)
import Snap.Snaplet (Handler, with)
import Snap.Snaplet.Auth (isLoggedIn, UserId(UserId), currentUser, AuthUser(userId))
import Snap.Snaplet.Heist (heistLocal, render, withSplices)
import Text.Digestive (Form)
import Text.Digestive.Form ((.:), text, check, checkM)
import Text.Digestive.Heist (digestiveSplices, bindDigestiveSplices)
import Text.Digestive.Snap (runForm, runFormWith, defaultSnapFormConfig, uploadPolicy, partPolicy)
import Text.Digestive.Types(Result(..))
import Snap.Extras.FlashNotice (flashSuccess, flashError)
import Data.Text.Encoding (decodeUtf8)
import Text.Digestive.View (View)
import Snap.Util.FileUploads (setMaximumFormInputSize, allowWithMaximumSize, defaultUploadPolicy)

import Application (App, AppHandler, auth, sess)
import Types (Source(..), NewSource(..), Dataset(..))
import Forms.Source (sourceForm)
import Utils (ensureLoggedIn)
import Mixpanel (track)
import Store.DDB.Source (createSource, putSource, getSources, getSource)
import Logger.Types (ResourceType(Source, Dataset), Action(Create, Update))

sourcesHandler :: AppHandler ()
sourcesHandler = method GET listSourcesHandler <|> method POST sourceFormHandler

sourceHandler :: AppHandler ()
sourceHandler = method GET $ do
  maybeSourceId <- getParam "sourceId"
  case maybeSourceId of
    Nothing -> render "404"
    Just sourceId -> do
      eitherSource <- liftIO . getSource $ decodeUtf8 sourceId
      case eitherSource of
        Left err ->
          render "404"
        Right source ->
          withSplices (sourceSplices source) $ render "sources/show"
  where
    sourceSplices Source{sTitle, sDescription, sUrl, sDatasets} = do
      "source_title"        ## textSplice sTitle
      "source_description"  ## textSplice (fromMaybe "no description" sDescription)
      "source_website_url"  ## textSplice (fromMaybe "" sUrl)
      "source_datasets"     ## mapSplices datasetSplice sDatasets

      where
        datasetSplice Dataset{dsUuid, dsTitle, dsDescription, dsUrl} =
          runChildrenWithText $ do
            "dataset_title"       ## dsTitle
            "dataset_description" ## fromMaybe "" dsDescription
            "dataset_url"         ## dsUrl


newSourceHandler :: AppHandler ()
newSourceHandler = method GET sourceFormHandler


runForm' :: MonadSnap m => Text -> Form v m a -> m (View v, Maybe a)
runForm' = runFormWith (defaultSnapFormConfig { uploadPolicy = setMaximumFormInputSize tenmegs defaultUploadPolicy
                                              , partPolicy = const $ allowWithMaximumSize tenmegs})
  where tenmegs = 10 * 1024 * 1024

sourceFormHandler = ensureLoggedIn $ do
  maybeUser <- with auth currentUser
  uuid      <- liftIO nextRandom

  let (UserId uid) = fromMaybe (UserId "0") (userId =<< maybeUser)
  (view, result)  <- runForm' "source" $ sourceForm uuid uid
  case result of
    Just newSource@NewSource{nsTitle} -> do
      liftIO $ createSource newSource
      flashSuccess sess $ "Successfully added " <> nsTitle <> " source"
      liftIO $ logEvent Create Source
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
        sourceSplice Source{sUuid, sTitle, sDescription, sUrl, sDatasets} =
          runChildrenWithText $ do
            "title"       ## sTitle
            "description" ## fromMaybe "" sDescription
            "numDatasets" ## T.pack . show $ length sDatasets
            "sourceId"    ## toText sUuid

