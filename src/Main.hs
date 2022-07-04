{-# language QuasiQuotes     #-}
{-# language TemplateHaskell #-}

module Main where

import Control.Monad.Reader             (ask)
import Data.Coerce                      (coerce)
import Data.Text                        (Text)
import Network.Wai.Handler.Warp         (run)
import Servant                          (Handler, type (:>), Get, Context(EmptyContext)
                                        , NamedRoutes, ServerT, Proxy(Proxy), hoistServer
                                        , throwError, err404
                                        )
import Servant.API.Generic              ((:-))
import Servant.HTML.Blaze               (HTML)
import Servant.Server.Generic           (AsServerT, genericServeTWithContext)
import Text.Hamlet                      (Html, shamlet)
import GHC.Generics                     (Generic)


import Types -- Everything!


data Api mode = Api
  { home  :: mode :- Get '[HTML] Html
  , admin :: mode :- "admin" :> NamedRoutes AdminRoutes
  }
  deriving stock Generic


data AdminRoutes mode = AdminRoutes
  { adminHome :: mode :- Get '[HTML] Html
  }
  deriving stock Generic


secretThings :: [Text]
secretThings
    = [ "secret 1"
      , "secret 2, somewhat more secret than secret 1!"
      , "mundane secret."
      ]


server :: Api (AsServerT PageM)
server = Api
  { home  = homeHandler
  , admin = adminServer
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


main :: IO ()
main = do
  let context = EmptyContext

  let env = initialEnv
      nat :: PageM a -> Handler a
      nat = runPageM' env

  run 8083 $
    genericServeTWithContext nat server context
