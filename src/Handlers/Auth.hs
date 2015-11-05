{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Handlers.Auth where

import Control.Monad.IO.Class (liftIO)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Snap.Core
import Snap.Snaplet (Handler, with)
import Snap.Snaplet.Auth (AuthManager,
  loginByUsername, logout, Password(ClearText))
import Snap.Snaplet.Heist
import Heist
import Heist.Interpreted (bindSplices, textSplice)
import Text.Digestive.Snap (runForm)
import Snap.Extras.FlashNotice (flashError)

import Application (App, AppHandler, auth, sess)
import Forms.Login (loginForm)
import Types (Login(..))
import Utils (showForm)
import Handlers.Source (sourcesHandler)
import Mixpanel (track)

loginHandler :: AppHandler ()
loginHandler = do
    (form, login) <- runForm "form" loginForm
    maybe (showForm "auth/login" form) loginUserHandler login

loginUserHandler :: Login -> AppHandler ()
loginUserHandler Login{loginUsername, loginPassword, loginRemember} = do
    eitherUser <- with auth $ loginByUsername loginUsername password loginRemember
    case eitherUser of
      Left err -> do
        flashError sess "Invalid login / password combination"
        liftIO $ track "auth-invalid-login"
        redirect "/login"
      Right _ -> do
        liftIO $ track "auth-login"
        redirect "/"
  where
    password = ClearText $ T.encodeUtf8 loginPassword


logoutHandler :: AppHandler ()
logoutHandler = do 
    with auth logout
    liftIO $ track "auth-logout"
    {- heistLocal (bindString "message" message) $ render "sources" -}
    redirect "/"
  {- where -}
    {- message = "You are now logged out." -}


