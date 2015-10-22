{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Handlers.User where

import           Snap.Core (method, redirect, Method(GET, POST))
import           Snap.Snaplet (Handler)
import           Snap.Snaplet.Auth (registerUser, AuthManager)
import           Snap.Snaplet.Heist (render)

import Application (App)


handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET $ render "users/new"

handleUsers :: Handler App (AuthManager App) ()
handleUsers = method POST handleFormSubmit
  where
    handleFormSubmit = registerUser "login" "password" >> redirect "/"




