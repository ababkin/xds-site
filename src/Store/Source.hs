{-# LANGUAGE OverloadedStrings #-}

module Store.Source where

import Control.Monad (void)
import Snap.Snaplet.PostgresqlSimple (query_, execute, query)

import Application (AppHandler)
import Types (Source(..))

fetchAll :: AppHandler [Source]
fetchAll = query_ "SELECT * FROM sources"

store :: Source -> AppHandler ()
store = void . execute "INSERT INTO sources VALUES (?, ?, ?, ?, ?, ?)"
