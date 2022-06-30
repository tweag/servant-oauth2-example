{-# language TypeFamilies   #-}
{-# language NamedFieldPuns #-}

module Auth where

import Control.Monad.IO.Class                         (liftIO, MonadIO)
import Data.ByteString                                (ByteString)
import Data.Text                                      (Text)
import Data.Text.Encoding                             (decodeUtf8)
import Network.HTTP.Types                             (Status)
import Network.HTTP.Types                             (status200)
import Network.Wai                                    (Request)
import Network.Wai qualified                          as Wai
import Network.Wai.Middleware.Auth qualified          as Wai
import Network.Wai.Middleware.Auth.OAuth2.Github      (mkGithubProvider)
import Network.Wai.Middleware.Auth.Provider qualified as Wai
import Servant                                        (AuthProtect, Handler, err401, throwError)
import Servant.Server.Experimental.Auth               (AuthServerData, AuthHandler, mkAuthHandler)
import Web.Cookie                                     (SetCookie)

import Types
import Config


data Login = Login
      { location :: Text -- ^ The location we will redirect to.
      }

data Complete = Complete
      { cookie :: SetCookie
      }


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
      response <- runGithubAuth request (_oauth (_config env)) Nothing Nothing DoLogin
      let headers = Wai.responseHeaders response
          Just location = lookup "Location" headers
      pure $ Login (decodeUtf8 location)


completeContext :: Env -> AuthHandler Request Complete
completeContext env = mkAuthHandler f
  where
    f :: Request -> Handler Complete
    f request = do
      -- 1. Get the ident from Wai Github Auth Provider
      -- 2. Make a session cookie for them.
      -- 3. Return it!

      let success ident = pure $ Wai.responseLBS status200 [("Success", ident)] ""
          -- failure resultStatus x = error $ "Error: " <> show x
          failure resultStatus x = pure $ Wai.responseLBS resultStatus [("Failure", x)] ""

      response <- runGithubAuth request (_oauth (_config env)) (Just success) (Just failure) DoComplete

      -- Know: If the headers of response contain 'Success', then we're logged
      -- in.

      let headers = Wai.responseHeaders response

      case lookup "Success" headers of
        Nothing -> throwError err401
        Just ident -> do
          -- We're in!
          cookie <- liftIO buildSessionCookie
          pure (Complete cookie)


buildSessionCookie :: IO SetCookie
buildSessionCookie = undefined
