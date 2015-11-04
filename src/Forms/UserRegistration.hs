{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Forms.UserRegistration where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Text.Digestive (Form)
import Text.Digestive.Form ((.:), text, check, checkM)
import Network.HTTP.Client (HttpException, parseUrl)
import Network.HTTP (simpleHTTP, getRequest, getResponseCode)
import Snap.Snaplet.Auth (usernameExists) 
import Snap.Snaplet (with)
import qualified Text.Email.Validate as E

import Application (AppHandler, auth)
import Forms.Validators (isNotEmpty)
import Types (UserRegistration(..))

userRegistrationForm 
  :: Form Text AppHandler UserRegistration 
userRegistrationForm =
  UserRegistration  <$> "first_name"  .: text Nothing
                    <*> "last_name"   .: text Nothing
                    <*> "username"    .: validateUsername (text Nothing)
                    <*> "email"       .: validateEmail (text Nothing)
                    <*> "password" .: (fst <$> validatePassword)

  where
    validatePassword =
      check "Passwords do not match:" (uncurry (==)) passwordF
      where
        passwordF = (,) 
          <$> "password" .: check "Password must not be empty" isNotEmpty (text Nothing) 
          <*> "password_confirmation" .: text Nothing
        

    validateEmail =
      check "Email must have the correct format" (E.isValid . T.encodeUtf8) .
      check "Email must not be empty" isNotEmpty

    validateUsername = 
      checkM "Username is already taken" usernameUnused .
      check "Username must not be empty" isNotEmpty
      where
        usernameUnused = liftM not . with auth . usernameExists

