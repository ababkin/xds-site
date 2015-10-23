{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handlers.Source where

import Control.Applicative ((<$>), (<*>), (<|>), pure)
import Control.Arrow((>>>))
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Monoid (Monoid, mempty)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime, UTCTime)
import Data.UUID (UUID)
import Snap.Core
import Snap.Snaplet
{- import           Snap.Snaplet.Auth -}
import Snap.Snaplet.Heist
import Heist ((##))
import Heist.Interpreted (mapSplices, runChildrenWithText)
import Snap.Snaplet.PostgresqlSimple (query_, execute, query)
import Data.UUID.V4 (nextRandom)

import Text.Digestive.Types(Result(..))
import Text.Digestive (Form)
import Text.Digestive.Form ((.:), text, validate )
import Text.Digestive.Snap (runForm)
{- import Text.Digestive.Util -}
import Text.Digestive.Heist (digestiveSplices, bindDigestiveSplices)

import Application (App)
import Types (Source(Source, title, description, url))


sourceForm 
  :: (Monad m) 
  => UUID 
  -> UTCTime 
  -> Form Text m Source
sourceForm uuid timestamp = do
  Source  <$> pure uuid  
          <*> "title"       .: validate (notEmpty >>> eitherToResult) (text Nothing)
          <*> "description" .: text Nothing
          <*> "url"         .: validate (notEmpty >>> eitherToResult) (text Nothing)
          <*> pure timestamp
          <*> pure timestamp

eitherToResult :: Either a b -> Result a b
eitherToResult x = case x of
  Left x'  -> Error x'
  Right x' -> Success x'

notEmpty :: (Monoid a, Eq a) => a -> Either Text a
notEmpty x =
  if (x == mempty)
     then Left "is empty"
     else Right x

handleSources :: Handler App App ()
handleSources = method GET handleListSources <|> method POST handleFormSubmit
  where
    handleFormSubmit = do 
      uuid            <- liftIO nextRandom
      timestamp       <- liftIO getCurrentTime
      (view, result)  <- runForm "form" $ sourceForm uuid timestamp
      case result of
        Just newSource  -> do
          _ <- execute "INSERT INTO sources VALUES (?, ?, ?, ?, ?, ?)" newSource
          redirect "/"
          
          {- heistLocal (bindUser x) $ render "user" -}
        Nothing -> 
          heistLocal (bindDigestiveSplices view) $ render "sources/new"



handleNewSource :: Handler App App ()
handleNewSource = method GET $ render "sources/new"

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

