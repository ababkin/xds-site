{-# LANGUAGE OverloadedStrings #-}

module Utils where

import Data.Monoid ((<>))
import Control.Monad ((<=<), mplus)
import Snap.Core (method, redirect, Method(GET, POST))
import Text.Digestive.Heist (bindDigestiveSplices, digestiveSplices)
{- import Text.Digestive.View (View) -}
import Snap.Snaplet.Heist (heistLocal, render, withSplices)
import Data.Text (Text)
import qualified Data.ByteString.Char8 as BS8
import Snap.Snaplet (Handler, with)
import Snap.Snaplet.Auth (isLoggedIn)
{- import Heist.Interpreted (bindSplices, textSplice, bindStrings, runChildrenWithText) -}
import Heist.Interpreted (Splice, runChildren)
import Heist (Splices, (##), HeistT, getParamNode)
{- import Heist.Splices (getRefAttributes) -}
import Text.Digestive.Form.List
import Text.Digestive.View
import Data.Maybe (fromMaybe)
import qualified Text.XmlHtml          as X


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

  withSplices (digestiveSplices form <> extraDigestiveSplices form) . render $ BS8.pack template


-- taken from Text.Digestive.Heist 

extraDigestiveSplices :: Monad m => View Text -> Splices (Splice m)
extraDigestiveSplices view = do
    "dfIfErrors"            ## dfIfErrors view

dfIfErrors :: Monad m => View v -> Splice m
dfIfErrors view = do
    (ref, _) <- getRefAttributes $ Just ""
    if null (errors ref view)
        then return []
        else runChildren


getRefAttributes :: Monad m
                 => Maybe Text                       -- ^ Optional default ref
                 -> HeistT m m (Text, [(Text, Text)])  -- ^ (Ref, other attrs)
getRefAttributes defaultRef = do
    node <- getParamNode
    return $ case node of
        X.Element _ as _ ->
            let ref = fromMaybe (error $ show node ++ ": missing ref") $
                        lookup "ref" as `mplus` defaultRef
            in (ref, filter ((/= "ref") . fst) as)
        _                -> (error "Wrong type of node!", [])

-------------------------------------


ensureLoggedIn action = do  
  loggedIn <- with auth isLoggedIn
  if loggedIn
    then action
    else redirect "/login"

