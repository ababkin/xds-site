{-# LANGUAGE OverloadedStrings #-}

module Utils where

import Data.Monoid ((<>))
import Control.Monad ((<=<))
import Snap.Core (method, redirect, Method(GET, POST))
import Text.Digestive.Heist (bindDigestiveSplices, digestiveSplices)
import Text.Digestive.View (View)
import Snap.Snaplet.Heist (heistLocal, render, withSplices)
import Data.Text (Text)
import qualified Data.ByteString.Char8 as BS8
import Snap.Snaplet (Handler, with)
import Snap.Snaplet.Auth (isLoggedIn)
import Snap.Extras.FlashNotice (flashSplice)
import Heist ((##))
import Heist.Interpreted (bindSplices, textSplice, bindStrings, runChildrenWithText)

import Application (AppHandler, auth, sess)


showForm :: String -> View Text -> AppHandler ()
showForm template form =
  {- heistLocal splices . render $ BS8.pack template -}
  {- heistLocal flashSplices =<< (heistLocal (bindDigestiveSplices form) . render $ BS8.pack template) -}
  {- heistLocal (bindDigestiveSplices form <> flashSplices) . render $ BS8.pack template -}
  {- heistLocal (bindDigestiveSplices form) =<< (heistLocal flashSplices . render $ BS8.pack template) -}


  {- heistLocal (bindStrings flashSplices) . render $ BS8.pack template -}
  {- heistLocal (bindStrings $ flashSplice sess) . render $ BS8.pack template -}

  {- works: -}
  {- heistLocal (bindSplices flashSplices) . render $ BS8.pack template -}
  {- heistLocal (bindDigestiveSplices form) . render $ BS8.pack template -}
  {- withSplices flashSplices . render $ BS8.pack template -}

  withSplices (digestiveSplices form <> flashSplices) . render $ BS8.pack template


  {- where -}
    {- template = BS8.pack $ prefix ++ "/form" -}
  where
    flashSplices = do
            "flash" ## flashSplice sess

ensureLoggedIn action = do  
  loggedIn <- with auth isLoggedIn
  if loggedIn
    then action
    else redirect "/login"

