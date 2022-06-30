{-# language QuasiQuotes     #-}
{-# language TemplateHaskell #-}

module Main where

import Data.Text                        (Text)
import Network.Wai.Handler.Warp         (run)
import Servant                          (Handler, type (:>), Get, Context((:.), EmptyContext)
                                        , NoContent, Header, Headers, WithStatus, UVerb, StdMethod(GET)
                                        , AuthProtect, NamedRoutes, ServerT, Proxy(Proxy), throwError
                                        , err404, NoContent(NoContent), addHeader, respond
                                        , WithStatus(WithStatus))
import Servant.API.Generic              ((:-))
import Servant.HTML.Blaze               (HTML)
import Servant.Server                   (hoistServer)
import Servant.Server.Generic           (AsServerT, genericServeTWithContext)
import Text.Hamlet                      (Html, shamlet)
import Toml                             (decodeFileExact)
import GHC.Generics                     (Generic)
import Web.Cookie                       (SetCookie)


import Auth -- Everything!
import Types -- Everything!
import Config -- Everything!


data Api mode = Api
  { home  :: mode :- Get '[HTML] Html
  , about :: mode :- "about" :> Get '[HTML] Html
  , auth  :: mode :- "auth" :> "github" :> NamedRoutes OAuthRoutes
  , admin :: mode :- "admin" :> NamedRoutes AdminRoutes
  }
  deriving stock Generic


-- | These are the routes required by the "OAuth2" workflow.
--    -> Login -> [GitHub] -> ... <- /complete
data OAuthRoutes mode = OAuthRoutes
  { login :: mode :- AuthProtect "login" :> "login"
                :> UVerb 'GET '[HTML] '[ WithStatus 303 (Headers '[ Header "Location" Text ] NoContent) ]

  , complete :: mode :- AuthProtect "complete" :> "complete"
                :> UVerb 'GET '[HTML] '[ WithStatus 303 (Headers '[ Header "Location" Text, Header "Set-Cookie" SetCookie ] NoContent) ]
  }
  deriving stock Generic


data AdminRoutes mode = AdminRoutes
  { adminHome :: mode :- Get '[HTML] Html
  }
  deriving stock Generic


secretThings :: [Text]
secretThings
    = [ "secret 1"
      , "definitely more secret than secret 1!"
      , "mundane secret."
      ]


server :: Api (AsServerT PageM)
server = Api
  { home  = homeHandler
  , about = aboutHandler
  , admin = adminServer
  , auth  = authServer
  }


adminServer :: ServerT (NamedRoutes AdminRoutes) PageM
adminServer = ensureAdmin $ AdminRoutes
  { adminHome = adminHandler
  }


-- | We ensure that someone has access to (view) admin pages by providing a
-- function that converts them from an admin to a normal user.
ensureAdmin :: ServerT (NamedRoutes AdminRoutes) AdminPageM
            -> ServerT (NamedRoutes AdminRoutes) PageM
ensureAdmin = hoistServer (Proxy @(NamedRoutes AdminRoutes)) checkAdmin
  where
    isAdmin = False
    checkAdmin :: AdminPageM a -> PageM a
    checkAdmin (PageM' routes) = do
      if isAdmin
         then PageM' @'Anyone routes
         else throwError err404


homeHandler :: PageM Html
homeHandler = do
  pure $ [shamlet|
    <h3> Home
    <a href="/auth/github/login"> Login
    |]


aboutHandler :: PageM Html
aboutHandler = do
  pure $ [shamlet| <h3> About  |]


adminHandler :: AdminPageM Html
adminHandler = do
  pure $ [shamlet|
    <h3> Admin
    <hr>

    <b> Secrets

    <ul>
      $forall secret <- secretThings
        <li> #{secret}
  |]


redirect :: Text -> Headers '[ Header "Location" Text ] NoContent
redirect location = addHeader location NoContent


authServer :: OAuthRoutes (AsServerT PageM)
authServer = OAuthRoutes
  { login    = \(Login location) -> respond $ WithStatus @303 (redirect location)
  , complete = \Complete -> undefined
  }


main :: IO ()
main = do
  eitherConfig <- decodeFileExact configCodec ("./configs/config.live.toml")
  config <- either (\errors -> fail $ "unable to parse configuration: " <> show errors)
                   pure
                   eitherConfig

  let env = Env config
      context = loginContext env :. completeContext :. EmptyContext

  run 8083 $
    genericServeTWithContext nat server context
      where
        nat :: PageM a -> Handler a
        nat = getPageM'
