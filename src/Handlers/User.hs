{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Handlers.User where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Applicative ((<$>), (<*>), (<|>), pure)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS8
{- import System.Random (newStdGen, randomRs) -}
import Data.Aeson (Value(String))

import Snap.Core (method, redirect, Method(GET, POST))
import Snap.Snaplet (Handler, with)
import Snap.Snaplet.Auth (createUser, AuthManager, saveUser, userRoles, 
  Role(Role), userEmail, userMeta)
import Text.Digestive (Form)
import Text.Digestive.Form ((.:), text, check, validateM)
import Text.Digestive.Snap (runForm)
import Data.Text (Text)
import Snap.Snaplet.Heist (heistLocal, render)
import Heist.Interpreted (bindSplices, textSplice, bindStrings)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Text.Digestive.Heist (digestiveSplices, bindDigestiveSplices)
import Data.Text.Encoding (encodeUtf8)
import Heist ((##))
import qualified Data.HashMap.Strict as HM
import Snap.Extras.FlashNotice (flashSuccess)

import Application (App, AppHandler, auth, sess)
import Types (UserRegistration(..))
import Forms.UserRegistration (userRegistrationForm)
import Utils (showForm)
import Mixpanel (track)

registrationHandler :: AppHandler ()
registrationHandler = do 
    (form, userRegistration) <- runForm "form" userRegistrationForm
    maybe (showForm "users/new" form) createNewUser userRegistration

  where
    createNewUser :: UserRegistration -> AppHandler ()
    createNewUser UserRegistration{urFirstName, urLastName, urUsername, urEmail, urPassword} = do
        {- password <- liftIO createRandomPassword -}
        eitherUser <- with auth $ createUser urUsername $ T.encodeUtf8 urPassword
        case eitherUser of
          Right user -> do
            void . with auth . saveUser $ user { 
                userRoles = [Role "Regular"] 
              , userEmail = Just urEmail
              , userMeta = HM.fromList [("firstName", String urFirstName), ("lastName", String urLastName)]
              }
            {- heistLocal (bindStrings messages) $ render "users/registration-done" -}
            flashSuccess sess "Registration successful, please login now"
            liftIO $ track "user-create"
            redirect "/login"


          Left err ->
            error $ show err
      {- where -}
        {- messages = do -}
          {- "firstName"  ## urFirstName -}
          {- "lastName"   ## urLastName -}

{- createRandomPassword :: IO ByteString -}
{- createRandomPassword = do -}
    {- gen <- newStdGen -}
    {- let pw = take 10 $ randomRs ('a', 'z') gen -}
    {- return $ BS8.pack pw -}

