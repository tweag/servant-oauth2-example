{-# language QuasiQuotes     #-}
{-# language TemplateHaskell #-}

module Main where

import Control.Monad.Reader             (ask, withReaderT)
import Data.Coerce                      (coerce)
import Data.Text                        (Text)
import GHC.Generics                     (Generic)
import Network.Wai.Handler.Warp         (run)
import Servant                          (Handler, type (:>), Get, Context(EmptyContext)
                                        , NamedRoutes, ServerT, Proxy(Proxy), hoistServer
                                        , throwError, err404, AuthProtect, Context( (:.) )
                                        , NoContent(NoContent), Header, Headers, WithStatus(WithStatus)
                                        , respond, addHeader, UVerb, StdMethod(GET)
                                        )
import Servant.API.Generic              ((:-))
import Servant.HTML.Blaze               (HTML)
import Servant.Server.Generic           (AsServerT, genericServeTWithContext)
import Text.Hamlet                      (Html, shamlet)
import Toml                             (decodeFileExact)
import Web.ClientSession                (getDefaultKey)
import Web.Cookie                       (SetCookie)


import Auth -- Everything!
import Types -- Everything!
import Config -- Everything!


data AllRoutes mode = AllRoutes
  { site :: mode :- AuthProtect "optional-cookie-auth" :> NamedRoutes SiteRoutes
  , auth :: mode :- "auth" :> "github" :> NamedRoutes OAuthRoutes
  }
  deriving stock Generic


data OAuthRoutes mode = OAuthRoutes
  { login :: mode :- AuthProtect "login" :> "login"
                :> UVerb 'GET '[HTML]
                    '[ WithStatus 303 (Headers '[ Header "Location" Text ] NoContent) ]

  , complete :: mode :- AuthProtect "complete" :> "complete"
                :> UVerb 'GET '[HTML]
                    '[ WithStatus 303 (Headers '[ Header "Location" Text
                                                , Header "Set-Cookie" SetCookie ] NoContent)
                     ]
  }
  deriving stock Generic


authServer :: OAuthRoutes (AsServerT PageM)
authServer = OAuthRoutes
  { login    = \(Login l) -> respond $ WithStatus @303 (redirect l)
  , complete = \(Complete c) -> respond $ WithStatus @303 (redirectWithCookie "/" c)
  }


redirect :: Text -> Headers '[ Header "Location" Text ] NoContent
redirect l = addHeader l NoContent


redirectWithCookie :: Text
                   -> SetCookie
                   -> Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] NoContent
redirectWithCookie destination c =
  addHeader destination (addHeader c NoContent)


data SiteRoutes mode = SiteRoutes
  { home  :: mode :- Get '[HTML] Html
  , admin :: mode :- "admin" :> NamedRoutes AdminRoutes
  }
  deriving stock Generic


data AdminRoutes mode = AdminRoutes
  { adminHome :: mode :- Get '[HTML] Html
  }
  deriving stock Generic


siteServer :: SiteRoutes (AsServerT PageM)
siteServer = SiteRoutes
  { home  = homeHandler
  , admin = adminServer
  }


server :: AllRoutes (AsServerT PageM)
server = AllRoutes
  { site = \sessionWithUser ->
      let addSession env = env { session = Just sessionWithUser }
       in hoistServer
            (Proxy @(NamedRoutes SiteRoutes))
            (\(PageM' p) -> PageM' $ withReaderT addSession p)
            siteServer
  , auth = authServer
  }


adminServer :: ServerT (NamedRoutes AdminRoutes) PageM
adminServer = ensureAdmin $ AdminRoutes
  { adminHome = adminHandler
  }


ensureAdmin :: ServerT (NamedRoutes AdminRoutes) AdminPageM
            -> ServerT (NamedRoutes AdminRoutes) PageM
ensureAdmin = hoistServer (Proxy @(NamedRoutes AdminRoutes)) transform
  where
    isAdmin :: Maybe User -> Bool
    -- TODO: Try changing this to be `True` !
    isAdmin _ = False

    transform :: AdminPageM a -> PageM a
    transform p = do
      env <- ask
      let currentUser = user =<< session env
      if isAdmin currentUser
         then coerce p
         else throwError err404


homeHandler :: PageM Html
homeHandler = do
  pure $ [shamlet|
    <h3> Home
    <a href="/admin"> Admin
    |]


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


secretThings :: [Text]
secretThings
    = [ "secret 1"
      , "secret 2, somewhat more secret than secret 1!"
      , "mundane secret."
      ]


main :: IO ()
main = do

  eitherConfig <- decodeFileExact configCodec ("./configs/config.live.toml")
  config' <- either (\errors -> fail $ "unable to parse configuration: " <> show errors)
                   pure
                   eitherConfig
  key <- getDefaultKey

  let env = initialEnv config' key
      nat :: PageM a -> Handler a
      nat = runPageM' env

  let context = loginAuthHandler env :. completeAuthHandler env :. authHandler env :. EmptyContext

  run 8083 $
    genericServeTWithContext nat server context
