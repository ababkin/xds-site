{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Forms.Login where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import Text.Digestive (Form)
import Text.Digestive.Form ((.:), text, bool, check, checkM)
import Snap.Snaplet (Handler, with)
import Snap.Snaplet.Auth (lookupByLogin, authenticatePassword, Password(ClearText))
import Data.Maybe (isNothing)

import Application (App, AppHandler, auth)
import Forms.Validators (isFormattedLikeURL, isNotEmpty)
import Types (Login(..))
import Forms.Validators (isNotEmpty)


loginForm :: Form Text AppHandler Login
loginForm =
    checkM invalidLoginMsg validLogin $ Login
      <$> "username" .: check "Username must not be empty" isNotEmpty (text Nothing)
      <*> "password" .: check "Password must not be empty" isNotEmpty (text Nothing)
      <*> "remember" .: bool (Just False)

  where

    validLogin :: Login -> AppHandler Bool
    validLogin Login{loginUsername, loginPassword, loginRemember} = do
        authMgr  <- with auth get
        authUser <- liftIO $ lookupByLogin authMgr loginUsername
        return $ maybe False authenticate authUser
      where
        authenticate = isNothing . flip authenticatePassword password
        password = ClearText $ T.encodeUtf8 loginPassword

    invalidLoginMsg = "Wrong username/password combination"

