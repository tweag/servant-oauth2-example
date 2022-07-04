{-# language QuasiQuotes     #-}
{-# language TemplateHaskell #-}
{-# language TypeFamilies    #-}

module Main where

import Control.Monad.Reader             (ask, withReaderT)
import Data.Coerce                      (coerce)
import Data.Text                        (Text)
import GHC.Generics                     (Generic)
import Network.Wai                      (Request)
import Network.Wai.Handler.Warp         (run)
import Servant                          (Handler, type (:>), Get, Context(EmptyContext)
                                        , NamedRoutes, ServerT, Proxy(Proxy), hoistServer
                                        , throwError, err404, AuthProtect, Context( (:.) )
                                        )
import Servant.API.Generic              ((:-))
import Servant.HTML.Blaze               (HTML)
import Servant.Server.Experimental.Auth (AuthServerData, AuthHandler)
import Servant.Server.Generic           (AsServerT, genericServeTWithContext)
import Text.Hamlet                      (Html, shamlet)


import Types -- Everything!


data AllRoutes mode = AllRoutes
  { site :: mode :- AuthProtect "optional-cookie-auth" :> NamedRoutes SiteRoutes
  }
  deriving stock Generic


type instance AuthServerData (AuthProtect "optional-cookie-auth") = Session 'Anyone


authHandler :: AuthHandler Request (Session 'Anyone)
authHandler = undefined


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
  let context = authHandler :. EmptyContext

  let env = initialEnv
      nat :: PageM a -> Handler a
      nat = runPageM' env

  run 8083 $
    genericServeTWithContext nat server context
