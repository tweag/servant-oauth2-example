{-# language TypeFamilies   #-}
{-# language NamedFieldPuns #-}

module Auth where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.ByteString (ByteString)
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types (Status)
import Network.Wai (Request)
import Network.Wai qualified as Wai
import Network.Wai.Middleware.Auth qualified as Wai
import Network.Wai.Middleware.Auth.OAuth2.Github (mkGithubProvider)
import Network.Wai.Middleware.Auth.Provider qualified as Wai
import Servant (AuthProtect)
import Servant (Handler)
import Servant.Server.Experimental.Auth (AuthServerData, AuthHandler, mkAuthHandler)

import Config
import Types

type instance AuthServerData (AuthProtect "optional-cookie-auth") = Session 'Anyone
type instance AuthServerData (AuthProtect "login") = Login
type instance AuthServerData (AuthProtect "complete") = Complete


data OAuthAction = DoLogin | DoComplete


authHandler :: AuthHandler Request (Session 'Anyone)
authHandler = undefined


loginAuthHandler :: Env r -> AuthHandler Request Login
loginAuthHandler env = mkAuthHandler f
  where
    f :: Request -> Handler Login
    f request = do
      response <- runGithubAuth request (_oauth (config env)) Nothing Nothing DoLogin
      let headers = Wai.responseHeaders response
          Just location = lookup "Location" headers
      pure $ Login (decodeUtf8 location)


completeHandler :: AuthHandler Request Complete
completeHandler = undefined


runGithubAuth :: (MonadIO m)
              => Request
              -> OAuthConfig
              -> Maybe (Wai.AuthLoginState -> IO Wai.Response)
              -> Maybe (Status -> ByteString -> IO Wai.Response)
              -> OAuthAction
              -> m Wai.Response
runGithubAuth request OAuthConfig{_secret,_id,_name} msuccess mfailure action = do
  let emailAllowList = [".*"]
      appRoot  = Wai.smartAppRoot request
      provider = Wai.Provider $ mkGithubProvider _name _id _secret emailAllowList Nothing
      success  = maybe (error "'success' not implemented") id msuccess
      failure  = maybe (error "'failure' not implemented") id mfailure
      suffix = case action of
                DoLogin    -> []
                DoComplete -> ["complete"]
      providerUrl (Wai.ProviderUrl url) = Wai.mkRouteRender (Just appRoot) "auth" url provider
  liftIO $ Wai.handleLogin provider request suffix providerUrl success failure

