{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Handlers.Source where

import Control.Applicative ((<|>))
import           Snap.Core
import           Snap.Snaplet
{- import           Snap.Snaplet.Auth -}
import           Snap.Snaplet.Heist
import           Heist
import Heist.Interpreted (mapSplices, runChildrenWithText)
import Snap.Snaplet.PostgresqlSimple (query_, execute)


import Application (App)
import Types (Source(Source, title, description, url))


handleSources :: Handler App App ()
handleSources = method GET handleListSources <|> method POST handleFormSubmit
  where
    handleFormSubmit = do 
      title       <- getPostParam "title"
      description <- getPostParam "description"
      url         <- getPostParam "url"
      _newSource <- execute "INSERT INTO sources VALUES (?, ?, ?)" (title, description, url)
      redirect "/"

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

