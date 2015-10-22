{-# LANGUAGE OverloadedStrings #-}

module Handlers.Auth where

import           Data.Monoid
import Data.Text (Text)
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth (AuthManager,
  loginUser, logout)
import           Snap.Snaplet.Heist
import           Heist
import qualified Heist.Interpreted as I

import Application (App)


handleLogin :: Maybe Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "auth/login"
  where
    errs = maybe mempty splice authError
    splice err = "loginError" ## I.textSplice err


handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
    loginUser "login" "password" Nothing
              (\_ -> handleLogin err) (redirect "/")
  where
    err = Just "Unknown user or password"


handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"



