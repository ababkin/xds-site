{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Site
  ( app
  ) where

------------------------------------------------------------------------------
import Control.Applicative ((<|>))
import Data.ByteString (ByteString)

import Snap.Core (method, Method(GET), ifTop)
import Snap.Util.FileServe (serveDirectory)

import Snap.Snaplet (wrapSite, Handler, with, makeSnaplet, 
  nestSnaplet, addRoutes, SnapletInit)
import Snap.Snaplet.Auth (loginByRememberToken, addAuthSplices)
import Snap.Snaplet.Heist (heistInit)
import Snap.Snaplet.Session.Backends.CookieSession
import Snap.Snaplet.Sass (sassServe, initSass)
import Snap.Snaplet.Auth.Backends.PostgresqlSimple (initPostgresAuth)
import Snap.Snaplet.PostgresqlSimple (pgsInit)

import Application (App, AppHandler, 
  auth, db, sess, heist, sass)
import Types (Source(title, description, url))
import Handlers.Auth (handleLogin, handleLoginSubmit, handleLogout)
import Handlers.Source (handleSources, handleListSources, handleNewSource)
import Handlers.User (handleNewUser, handleUsers)

import Application (App(App))


routes :: [(ByteString, Handler App App ())]
routes =  [ ("/sources",      handleSources)
          , ("/sources/new",  handleNewSource)
          
          , ("/users",        with auth handleUsers)
          , ("/signup",       with auth handleNewUser)

          , ("/login",        with auth handleLoginSubmit)
          , ("/logout",       with auth handleLogout)

          , ("/sass",         with sass sassServe)
          , ("/static",       serveDirectory "static")
          ]


app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h     <- nestSnaplet "" heist $ heistInit "templates"

    se    <- nestSnaplet "sess" sess $ initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    d     <- nestSnaplet "db" db pgsInit

    a     <- nestSnaplet "auth" auth $ initPostgresAuth sess d

    sa    <- nestSnaplet "sass" sass initSass
  
    addRoutes routes
    addAuthSplices h auth
    wrapSite (\h -> with auth loginByRememberToken >> h)
    {- wrapSite (<|> the404) -}
    wrapSite (\site -> ifTop handleListSources <|> site)
    
    return $ App h se a d sa


  

  


