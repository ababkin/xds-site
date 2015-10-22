{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import Control.Monad.IO.Class (liftIO)
import           Data.ByteString (ByteString)
import           Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import           Snap.Core
import           Snap.Snaplet (wrapSite, Handler, with, makeSnaplet, 
  nestSnaplet, addRoutes, SnapletInit)
import           Snap.Snaplet.Auth (loginByRememberToken, addAuthSplices)
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Heist
import qualified Heist.Interpreted as I
import Snap.Snaplet.Sass
import Snap.Snaplet.Auth.Backends.PostgresqlSimple (initPostgresAuth)
import Snap.Snaplet.PostgresqlSimple (pgsInit)

import Application (App, AppHandler, 
  auth, db, sess, heist, sass)
import Types (Source(title, description, url))
import Handlers.Auth (handleLogin, handleLoginSubmit, handleLogout)
import Handlers.Source (handleSources, handleListSources, handleNewSource)
import Handlers.User (handleNewUser, handleUsers)

import Application (App(App))


handleRoot :: Handler App App ()
handleRoot = method GET handleListSources


routes :: [(ByteString, Handler App App ())]
routes =  [ ("",              handleRoot)
          , ("/sources",      handleSources)
          , ("/sources/new",  handleNewSource)
          
          , ("/users",        with auth handleUsers)
          , ("/users/new",    with auth handleNewUser)

          , ("/login",        with auth handleLoginSubmit)
          , ("/logout",       with auth handleLogout)

          , ("/sass",         with sass sassServe)
          , ("/static",       serveDirectory "static")
          ]


app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h     <- nestSnaplet "" heist $ heistInit "templates"

    se     <- nestSnaplet "sess" sess $ initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    d    <- nestSnaplet "db" db pgsInit

    a     <- nestSnaplet "auth" auth $ initPostgresAuth sess d

    sa  <- nestSnaplet "sass" sass initSass
  
    addRoutes routes
    addAuthSplices h auth
    wrapSite (\h -> with auth loginByRememberToken >> h)
    
    return $ App h se a d sa


  

  


