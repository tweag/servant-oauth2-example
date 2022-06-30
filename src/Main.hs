{-# language QuasiQuotes     #-}
{-# language TemplateHaskell #-}

module Main where

import Data.Text                        (Text)
import Network.Wai.Handler.Warp         (run)
import Servant                          (Handler, type (:>), Get, Context((:.), EmptyContext)
                                        , NoContent, Header, Headers, WithStatus, UVerb, StdMethod(GET)
                                        , AuthProtect, NamedRoutes)
import Servant.API.Generic              ((:-))
import Servant.HTML.Blaze               (HTML)
import Servant.Server.Generic           (AsServerT, genericServeTWithContext)
import Text.Hamlet                      (Html, shamlet)
import GHC.Generics                     (Generic)
import Web.Cookie                       (SetCookie)


import Auth -- Everything!
import Types -- Everything!


data Api mode = Api
  { home  :: mode :- Get '[HTML] Html
  , about :: mode :- "about" :> Get '[HTML] Html
  , auth  :: mode :- "auth" :> "github" :> NamedRoutes OAuthRoutes
  , admin :: mode :- "admin" :> Get '[HTML] Html
  }
  deriving stock Generic


-- | These are the routes required by the "OAuth2" workflow.
--    -> Login -> [GitHub] -> ... <- /complete
data OAuthRoutes mode = OAuthRoutes
  { login :: mode :- AuthProtect "login" :> "login"
                :> UVerb 'GET '[HTML] '[ WithStatus 301 (Headers '[ Header "Location" Text ] NoContent) ]

  , complete :: mode :- AuthProtect "complete" :> "complete"
                :> UVerb 'GET '[HTML] '[ WithStatus 301 (Headers '[ Header "Location" Text, Header "Set-Cookie" SetCookie ] NoContent) ]
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
  { home  = pure $ [shamlet| <h3> Home  |]
  , about = pure $ [shamlet| <h3> About |]
  -- Terrible! We've leaked our secrets?!
  , admin = pure $
              [shamlet|
                <h3> Admin
                <hr>

                <b> Secrets

                <ul>
                  $forall secret <- secretThings
                    <li> #{secret}
              |]
  , auth  = authServer
  }


redirect location = undefined


authServer :: OAuthRoutes (AsServerT PageM)
authServer = OAuthRoutes
  { login    = \(Login location) -> redirect location
  , complete = \Complete -> undefined
  }


main :: IO ()
main = do
  run 8083 $
    genericServeTWithContext nat server context
      where
        env = undefined
        context = loginContext env :. completeContext :. EmptyContext
        nat = id
