{-# LANGUAGE OverloadedStrings #-}

module Utils where

import Snap.Core (method, redirect, Method(GET, POST))
import Text.Digestive.Heist (bindDigestiveSplices)
import Text.Digestive.View (View)
import Snap.Snaplet.Heist (heistLocal, render)
import Data.Text (Text)
import qualified Data.ByteString.Char8 as BS8
import Snap.Snaplet (Handler, with)
import Snap.Snaplet.Auth (isLoggedIn)

import Application (AppHandler, auth)


showForm :: String -> View Text -> AppHandler ()
showForm template form =
    heistLocal (bindDigestiveSplices form) . render $ BS8.pack template
  {- where -}
    {- template = BS8.pack $ prefix ++ "/form" -}

ensureLoggedIn action = do  
  loggedIn <- with auth isLoggedIn
  if loggedIn
    then action
    else redirect "/login"

