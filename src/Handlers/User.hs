{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Handlers.User where

import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist

import Application (App)


handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET $ render "users/index"

handleUsers :: Handler App (AuthManager App) ()
handleUsers = method POST handleFormSubmit
  where
    handleFormSubmit = registerUser "login" "password" >> redirect "/"




