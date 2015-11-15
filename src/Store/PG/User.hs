{-# LANGUAGE OverloadedStrings #-}

module Store.PG.User where

import Control.Monad (void)
import Snap.Snaplet.PostgresqlSimple (query, execute, query)
import Data.Text.Read

import Application (AppHandler)
import Types (User(..))
import Store.PG.Types ()


fetch :: Text -> AppHandler User
fetch tuid = case decimal tuid of
    Right (uid, "") ->
      query "SELECT uid, login, email FROM snap_auth_user WHERE uid = ?" uid
    Right (_, _) ->
      error "Could not completely parse user id into an integer value"
    Left err ->
      error $ "Error parsing text user id: " ++ show tuid ++ ", error was: " ++ err
      
