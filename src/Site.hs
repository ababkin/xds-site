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
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Heist
import qualified Heist.Interpreted as I
import qualified Text.XmlHtml as X
import Snap.Snaplet.PostgresqlSimple (pgsInit, query_, execute)
import Database.PostgreSQL.Simple.FromRow
import Snap.Snaplet.Sass
import Snap.Snaplet.Auth.Backends.PostgresqlSimple (initPostgresAuth)

------------------------------------------------------------------------------
import           Application


data Source = Source {
    title       :: Text
  , description :: Text
  , url         :: Text
  } deriving Show

instance FromRow Source where
  fromRow = Source 
                <$> field 
                <*> field
                <*> field

handleRoot :: Handler App App ()
handleRoot = method GET handleListSources

handleSources :: Handler App App ()
handleSources = method GET handleListSources <|> method POST handleFormSubmit
  where
    handleFormSubmit = do 
      title       <- getPostParam "title"
      description <- getPostParam "description"
      url         <- getPostParam "url"
      newProject <- execute "INSERT INTO sources VALUES (?, ?, ?)" (title, description, url)
      redirect "/sources"

handleNewSource :: Handler App App ()
handleNewSource = method GET $ render "sources/new"

handleListSources :: Handler App App ()
handleListSources = do
  sources <- query_ "SELECT * FROM sources"
  withSplices (sourcesSplices sources) $ render "sources/index"
  
  where
    sourcesSplices sources = do
      "sources" ## I.mapSplices gen sources
      where
        gen Source{title, description, url} =
          I.runChildrenWith $ do
            "title"       ## I.textSplice title
            "description" ## I.textSplice description
            "url"         ## I.textSplice url



handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET $ render "users/index"

handleUsers :: Handler App (AuthManager App) ()
handleUsers = method POST handleFormSubmit
  where
    handleFormSubmit = registerUser "login" "password" >> redirect "/"




------------------------------------------------------------------------------
-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "auth/login"
  where
    errs = maybe mempty splice authError
    splice err = "loginError" ## I.textSplice err


------------------------------------------------------------------------------
-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
    loginUser "login" "password" Nothing
              (\_ -> handleLogin err) (redirect "/")
  where
    err = Just "Unknown user or password"


------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"





------------------------------------------------------------------------------
-- | The application's routes.
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


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"

    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    db <- nestSnaplet "db" db pgsInit

    a <- nestSnaplet "auth" auth $ initPostgresAuth sess db

    sass <- nestSnaplet "sass" sass initSass
  

    addRoutes routes
    addAuthSplices h auth
    
    return $ App h s a db sass


  

  


