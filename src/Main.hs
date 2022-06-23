{-# language QuasiQuotes     #-}
{-# language TemplateHaskell #-}
{-# language TypeFamilies    #-}

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
import Servant.Server.Experimental.Auth (AuthServerData, AuthHandler)
import Network.Wai                      (Request)

type PageM = Handler

data Api mode = Api
  { home  :: mode :- Get '[HTML] Html
  , about :: mode :- "about" :> Get '[HTML] Html
  , auth :: mode :- "auth" :> "github" :> NamedRoutes AuthRoutes
  }
  deriving stock Generic

data Login = Login

data AuthRoutes mode = AuthRoutes
  { login :: mode :- AuthProtect "login" :> "login" :> UVerb 'GET '[HTML] '[ WithStatus 301 (Headers '[ Header "Location" Text ] NoContent) ]
  }
  deriving stock Generic

type instance AuthServerData (AuthProtect "login") = Login


server :: Api (AsServerT PageM)
server = Api
  { home  = pure $ [shamlet| <p> Home |]
  , about = pure $ [shamlet| <p> About |]
  , auth  = authServer
  }

authServer :: AuthRoutes (AsServerT PageM)
authServer = AuthRoutes
  { login = \Login -> undefined
  }

loginContext :: AuthHandler Request Login
loginContext = undefined

main :: IO ()
main = do
  run 8083 $
    genericServeTWithContext nat server context
      where
        context = loginContext :. EmptyContext
        nat = id
