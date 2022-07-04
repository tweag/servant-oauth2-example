{-# language QuasiQuotes     #-}
{-# language TemplateHaskell #-}

module Main where

import Data.Text                        (Text)
import Control.Monad.Reader             (runReaderT)
import Network.Wai.Handler.Warp         (run)
import Servant                          (Handler, type (:>), Get, Context(EmptyContext)
                                        , NamedRoutes, ServerT)
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
adminServer = AdminRoutes
  { adminHome = adminHandler
  }


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
      nat = flip runReaderT env

  run 8083 $
    genericServeTWithContext nat server context
