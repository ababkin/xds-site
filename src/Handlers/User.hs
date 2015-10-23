{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Handlers.User where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Applicative ((<$>), (<*>), (<|>), pure)
import           Snap.Core (method, redirect, Method(GET, POST))
import           Snap.Snaplet (Handler)
import           Snap.Snaplet.Auth (createUser, AuthManager)
import Text.Digestive (Form)
import Text.Digestive.Form ((.:), text, check, validateM)
import Text.Digestive.Snap (runForm)
import Data.Text (Text)
import Snap.Snaplet.Heist (heistLocal, render)
import qualified Data.Text as T
import Text.Digestive.Heist (digestiveSplices, bindDigestiveSplices)
import Data.Text.Encoding (encodeUtf8)

import Application (App)
import Types (UserRegistration(..))


registrationForm 
  :: (Monad m, MonadIO m) 
  => Form Text m UserRegistration 
registrationForm = do
  UserRegistration  <$> "first_name"  .: text Nothing
                    <*> "last_name"   .: text Nothing
                    <*> "login"       .: check "Login must not be empty" isNotEmpty (text Nothing)
                    <*> "email"       .: check "Email must not be empty" isNotEmpty (text Nothing)
                    <*> "password"              .: check "Password must not be empty" isNotEmpty (text Nothing)
                    <*> "password_confirmation" .: text Nothing

isNotEmpty :: Text -> Bool
isNotEmpty = not . T.null


handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleRegistrationForm

handleUsers :: Handler App (AuthManager App) ()
handleUsers = method POST handleRegistrationForm

handleRegistrationForm = do
  (view, result)  <- runForm "user" registrationForm
  case result of
    Just UserRegistration{login, password} -> do
      createUser login $ encodeUtf8 password
      redirect "/"
    Nothing -> 
      heistLocal (bindDigestiveSplices view) $ render "users/new"


