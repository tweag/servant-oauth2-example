{-# language TypeFamilies   #-}
{-# language NamedFieldPuns #-}

module Auth where

import Control.Monad.IO.Class                         (liftIO, MonadIO)
import Data.ByteString                                (ByteString)
import Data.Text                                      (Text)
import Network.HTTP.Types                             (Status)
import Network.Wai                                    (Request)
import Servant                                        (AuthProtect, Handler)
import Servant.Server.Experimental.Auth               (AuthServerData, AuthHandler, mkAuthHandler)
import Network.Wai qualified                          as Wai
import Network.Wai.Middleware.Auth qualified          as Wai
import Network.Wai.Middleware.Auth.Provider qualified as Wai
import Network.Wai.Middleware.Auth.OAuth2.Github      (mkGithubProvider)
import Data.Text.Encoding                             (decodeUtf8)

import Types


data Login = Login
      { location :: Text -- ^ The location we will redirect to.
      }

data Complete = Complete


type instance AuthServerData (AuthProtect "login") = Login
type instance AuthServerData (AuthProtect "complete") = Complete


data OAuthAction = DoLogin | DoComplete


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


loginContext :: Env -> AuthHandler Request Login
loginContext env = mkAuthHandler f
  where
    f :: Request -> Handler Login
    f request = do
      response <- runGithubAuth request (_oauthConfig env) Nothing Nothing DoLogin
      let headers = Wai.responseHeaders response
          Just location = lookup "Location" headers
      pure $ Login (decodeUtf8 location)




completeContext :: AuthHandler Request Complete
completeContext = undefined
